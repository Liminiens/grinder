namespace Grinder.Commands

open System
open Grinder
open Grinder.Types
open FSharp.UMX
open Funogram.Api

module Parser =
    open FParsec

    type Usernames = Usernames of string list

    type TimeFraction =
        | Minutes of uint32
        | Days of uint32
        | Months of uint32
    
    type BanDuration =
        | Forever
        | Timed of DateTime
        
    type TimeFractionSummator() =
        let mutable value = Unchecked.defaultof<DateTime>
        
        member __.IsSet() =
            value <> Unchecked.defaultof<DateTime>
        
        member __.GetValue() =
            value
        
        member __.Add(fraction) =
            if not <| __.IsSet() then
                value <- DateTime.UtcNow
            match fraction with
            | Minutes mins ->
                value <- value.AddMinutes(float mins)
            | Days days ->
                value <- value.AddDays(float days)
            | Months months ->
                value <- value.AddMonths(int32 months)
                
    type CommandAction =
        | Ban of BanDuration
        | Unban

    type Command = Command of Usernames * CommandAction

    let str s = pstring s
        
    let pminutes: Parser<uint32, unit> =
        (puint32 .>> spaces) .>> (regex "min(s|utes)?")
        |>> (fun v -> if v < 5u then 6u else v)
        
    let pdays: Parser<uint32, unit> =
        (puint32 .>> spaces) .>> (regex "day(s)?")
        
    let pmonths: Parser<uint32, unit> =
        (puint32 .>> spaces) .>> (regex "month(s)?")
        
    let pusername: Parser<string, unit> =
        let validate char =
            (not <| Char.IsWhiteSpace(char)) && char <> '@'
        pipe2 (str "@") (manySatisfy validate) (+)

    let pbotUsername botUsername : Parser<string, unit> =
        spaces >>. (str botUsername) .>> spaces
      
    let many1Usernames: Parser<string list, unit> =
        many1 (pusername .>> spaces)
    
    let sumTimedFractions (fractions: TimeFraction list) =
        let summator = TimeFractionSummator()
        for fraction in fractions do
            summator.Add(fraction)
        Timed <| summator.GetValue()
        
    let pdistinctTimeFractions: Parser<BanDuration, unit> =
        [
            pminutes |>> Minutes .>> spaces;
            pmonths |>> Months .>> spaces;
            pdays |>> Days .>> spaces
        ]
        |> List.map attempt
        |> choice
        |> many
        |>> List.distinct
        |>> sumTimedFractions
    
    let pforeverBan: Parser<BanDuration, unit>  =
        [
            spaces >>. eof >>% Forever;
            spaces >>. str "forever" >>% Forever;
        ]
        |> List.map attempt
        |> choice
    
    let pban: Parser<BanDuration, unit> =
        str "ban" .>> spaces >>. (pforeverBan <|> pdistinctTimeFractions)

    let punban: Parser<CommandAction, unit> =
        str "unban" .>> spaces >>% Unban
        
    let pcommandAction: Parser<CommandAction, unit> =
        (pban |>> Ban) <|> punban

    let parseCommand botUsername =
        pbotUsername botUsername >>.
        pipe2 many1Usernames pcommandAction (fun usernames command -> Command(Usernames(usernames), command))
        
    let runCommandParser botUsername str: ParserResult<Command, unit> =
        run (parseCommand botUsername) str
    
    type CommandParsingResult =
        | BotCommand of Command
        | IgnoreCommand
    
    let parse botUsername text =
        match runCommandParser botUsername text with
        | Success(result, _, _)   ->
            BotCommand result
        | Failure(errorMsg, _, _) ->
            IgnoreCommand
            
module Processing =
    open Funogram
    open Funogram.Bot
    open Funogram.Types
    open Grinder.FunogramExt
    open Parser
    
    type UserMessageContext = {
        BotConfig: BotConfig
        BotUsername: UserUsername
        Message: Message
        MessageText: string
        FromUsername: UserUsername
        ChatUsername: ChatUsername
    }
    
    type ReplyToMessageContext = {
        BotConfig: BotConfig
        BotUsername: UserUsername
        Message: Message
        MessageText: string
        ReplyToUser: User
        ReplyToMessage: Message
        FromUsername: UserUsername
        ChatUsername: ChatUsername
    }
    
    let private (|CommandAllowed|CommandNotAllowed|) (botSettings: BotSettings, username, chatUsername) =
        let isAllowedUser username =
            botSettings.AllowedUsers.Set 
            |> Set.contains %username

        let isAllowedChat chatUsername =
            botSettings.ChatsToMonitor.Set 
            |> Set.contains %chatUsername
            
        if isAllowedUser username && isAllowedChat chatUsername then    
            CommandAllowed
        else
            CommandNotAllowed

    let private userCanBeBanned botSettings username =
        botSettings.AllowedUsers.Set
        |> Set.contains username

    let private deleteMessage context chatId messageId =
        Api.deleteMessage chatId messageId
        |> callApiWithDefaultRetry context
        |> Async.Ignore
    
    let private concatErrors results =
        let _, errors = 
            results
            |> Result.partition
        if Array.length errors > 0 then
            String.join "\n" errors
        else
            String.Empty
            
    let iterTextMessage fn (context: UpdateContext) (message: Message) =
        context.Me.Username
        |> Option.bind ^ fun botUsername ->
            message.From
            |> Option.map ^ fun from ->
                (botUsername, message, from)
        |> Option.bind ^ fun (botUsername, message, from) ->
            from.Username
            |> Option.map ^ fun username ->
                (botUsername, message, username)
        |> Option.bind ^ fun (botUsername, message, username) ->
            message.Text
            |> Option.map ^ fun text ->
                (botUsername, message, username, text)
        |> Option.bind ^ fun (botUsername, message, username, text) ->
            message.Chat.Username
            |> Option.map ^ fun chatUsername -> {
                BotConfig = context.Config
                BotUsername = %botUsername
                Message = message
                MessageText = text
                FromUsername = %username
                ChatUsername = %(sprintf "@%s" chatUsername)
            }
        |> Option.map fn
        |> Option.defaultValue Async.Unit
  
    let processTextCommand (botSettings: BotSettings) (context: UserMessageContext) = async {
        match (botSettings, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            let parsedMessage = Parser.parse %context.BotUsername context.MessageText
            match parsedMessage with
            | BotCommand(Command(Usernames(usernames), Ban(duration))) ->
                do! deleteMessage context.BotConfig context.Message.Chat.Id context.Message.MessageId
                
                let until =
                    match duration with
                    | Forever ->
                        DateTime.UtcNow.AddMonths(13)
                    | Timed date ->
                        date
                
                let! requestsResult = 
                    [for user in usernames do
                        if userCanBeBanned botSettings user then
                            for chat in botSettings.ChatsToMonitor.Set do
                                yield ApiExt.restrictUser context.BotConfig chat user until
                        else
                            yield sprintf "Cannot ban admin @%s" user
                                  |> (Result.Error >> async.Return)]
                    |> Async.Parallel
                    
                let message =
                    let durationText =
                        if until > DateTime.UtcNow.AddYears(1) then
                            "forever"
                        else
                            until.ToString("yyyy-MM-dd HH:mm:ss")
                            
                    let usernamesText =
                        usernames
                        |> String.join ", "
                    let chatsText =
                        botSettings.ChatsToMonitor.Set
                        |> String.join ", "
                    sprintf "Banned %s in chats %s until %s UTC" usernamesText chatsText durationText
                    
                do! concatErrors requestsResult
                    |> sprintf "Command from: @%s\n\n%s\n%s" %context.FromUsername message
                    |> ApiExt.sendMessage botSettings.ChannelId context.BotConfig
                    
            | BotCommand(Command(Usernames(usernames), Unban)) ->
                do! deleteMessage context.BotConfig context.Message.Chat.Id context.Message.MessageId
                
                let! requestsResult = 
                    [for chat in botSettings.ChatsToMonitor.Set do
                        for user in usernames do
                            yield ApiExt.unrestrictUser context.BotConfig chat user]
                    |> Async.Parallel
                    
                let message =
                    let usernamesText =
                        usernames
                        |> String.join ", "
                    let chatsText =
                        botSettings.ChatsToMonitor.Set
                        |> String.join ", "
                    sprintf "Unbanned %s in chats %s" usernamesText chatsText
                    
                do! concatErrors requestsResult
                    |> sprintf "Command from: @%s\n\n%s\n%s" %context.FromUsername message
                    |> ApiExt.sendMessage botSettings.ChannelId context.BotConfig
            | IgnoreCommand -> ()
        | CommandNotAllowed -> ()
    }
    
    let processAdminCommand (botSettings: BotSettings) (config: BotConfig) fileId = async {
        match! ApiExt.prepareAndDownloadFile config fileId with
        | Ok stream ->
            let users = JsonNet.deserializeFromStream<DataAccess.User[]>(stream)
            do! Datastore.upsertUsers users
            do! "Updated user database"
                |> ApiExt.sendMessage botSettings.ChannelId config
        | Error e ->
            do! sprintf "Failed to download file. Description: %s" e.Description
                |> ApiExt.sendMessage botSettings.ChannelId config
    }
    
    let iterReplyToMessage fn (context: UpdateContext) (reply: ReplyToMessage) =
        context.Me.Username
        |> Option.bind ^ fun botUsername ->
            reply.Message.From
            |> Option.map ^ fun from ->
                (botUsername, reply.Message, from)
        |> Option.bind ^ fun (botUsername, message, from) ->
            from.Username
            |> Option.map ^ fun username ->
                (botUsername, message, username)
        |> Option.bind ^ fun (botUsername, message, username) ->
            message.Text
            |> Option.map ^ fun text ->
                (botUsername, message, username, text)
        |> Option.bind ^ fun (botUsername, message, username, text) ->
            message.Chat.Username
            |> Option.map ^ fun chatUsername ->
                (chatUsername, botUsername, message, username, text)
        |> Option.bind ^ fun (chatUsername, botUsername, message, username, text) ->
            if reply.ReplyToMessage.From = reply.Message.From then
                reply.ReplyToMessage.NewChatMember
                |> Option.map ^ fun user -> 
                     { BotConfig = context.Config
                       BotUsername = %botUsername
                       Message = message
                       MessageText = text
                       ReplyToUser = user
                       ReplyToMessage = reply.ReplyToMessage
                       FromUsername = %username
                       ChatUsername = %(sprintf "@%s" chatUsername) }
            else
                match reply.ReplyToMessage.From with
                | Some from -> 
                  { BotConfig = context.Config
                    BotUsername = %botUsername
                    Message = message
                    MessageText = text
                    ReplyToUser = from
                    ReplyToMessage = reply.ReplyToMessage
                    FromUsername = %username
                    ChatUsername = %(sprintf "@%s" chatUsername) }
                  |> Some
                | None ->
                   reply.ReplyToMessage.NewChatMember
                   |> Option.map ^ fun user -> 
                        { BotConfig = context.Config
                          BotUsername = %botUsername
                          Message = message
                          MessageText = text
                          ReplyToUser = user
                          ReplyToMessage = reply.ReplyToMessage
                          FromUsername = %username
                          ChatUsername = %(sprintf "@%s" chatUsername) }
        |> Option.map fn
        |> Option.defaultValue Async.Unit
    
    let banUserOnReplyMessage (botSettings: BotSettings) (context: ReplyToMessageContext) = async {
        let getUsernameByIdForLogging userId = async {
            match! Datastore.findUsernameByUserId userId with
            | UsernameFound username ->
                return sprintf "@%s" username
            | UsernameNotFound ->
                return "unknown"
        }

        do! deleteMessage context.BotConfig context.ReplyToMessage.Chat.Id context.ReplyToMessage.MessageId
                
        let! requestsResult = 
            let banDuration = DateTime.UtcNow.AddMonths(13)
                    
            [for chat in botSettings.ChatsToMonitor.Set do
                yield ApiExt.restrictUserById context.BotConfig chat context.ReplyToUser.Id banDuration]
            |> Async.Parallel
                
        let! username = getUsernameByIdForLogging context.ReplyToUser.Id
                
        let message =
            let chatsText =
                botSettings.ChatsToMonitor.Set
                |> String.join ", "
            sprintf "Banned %i (%s) in chats %s forever" context.ReplyToUser.Id username chatsText
                    
        do! concatErrors requestsResult
            |> sprintf "Command from: @%s\n\n%s\n%s" %context.FromUsername message
            |> ApiExt.sendMessage botSettings.ChannelId context.BotConfig
    }

    let processReplyMessage (botSettings: BotSettings) (context: ReplyToMessageContext) = async {
        do! context.ReplyToUser.Username
            |> Option.map ^ fun username ->
                [DataAccess.User(UserId = context.ReplyToUser.Id, Username = username)]
                |> Datastore.upsertUsers
            |> Option.defaultValue Async.Unit
        match (botSettings, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            if context.MessageText.Contains("ban") && context.MessageText.Contains(%context.BotUsername) then
                do! deleteMessage context.BotConfig context.Message.Chat.Id context.Message.MessageId

                match context.ReplyToUser.Username with
                | Some username -> 
                    if userCanBeBanned botSettings username then
                        do! banUserOnReplyMessage botSettings context
                    else
                        do! sprintf "Cannot ban admin @%s" context.ReplyToUser.Username.Value 
                            |> ApiExt.sendMessage botSettings.ChannelId context.BotConfig
                | None -> 
                    do! banUserOnReplyMessage botSettings context
        | CommandNotAllowed -> ()
    }
    
    let processNewUsersCommand (users: Types.User seq) =
        users
        |> Seq.filter ^ fun u -> Option.isSome u.Username
        |> Seq.map ^ fun u ->
            DataAccess.User(UserId = u.Id, Username = Option.get u.Username)
        |> Datastore.upsertUsers
        