namespace Grinder.Commands

open System
open Grinder
open Grinder.Types
open FSharp.UMX
open Funogram.Api

module Parser =
    open FParsec

    type Usernames = Usernames of string list
    
    [<Measure>] type private parserError
    type ParserError = string<parserError>
    
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
        | InvalidCommand of ParserError
    
    let parse botUsername text =
        match runCommandParser botUsername text with
        | Success(result, _, _)   ->
            BotCommand result
        | Failure(errorMsg, _, _) ->
            InvalidCommand %errorMsg
            
module Processing =
    open Funogram
    open Funogram.Bot
    open Funogram.Types
    open Grinder.FunogramExt
    open Parser
    
    type UserTextMessageContext = {
        BotUsername: UserUsername
        Message: Message
        MessageText: string
        FromUsername: UserUsername
        ChatUsername: ChatUsername
    }
    
    type ReplyToMessageContext = {
        BotUsername: UserUsername
        Message: Message
        MessageText: string
        ReplyToUser: User
        ReplyToMessage: Message
        FromUsername: UserUsername
        ChatUsername: ChatUsername
    }
    
    type CommandValidationResult =
        | CommandAllowed
        | CommandNotAllowed
    
    let isCommandAllowed (botSettings: BotSettings) username chatUsername =
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
            
    let prepareTextMessage (botUsername: string option) (message: Message) =
        botUsername
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
                BotUsername = %botUsername
                Message = message
                MessageText = text
                FromUsername = %username
                ChatUsername = %(sprintf "@%s" chatUsername)
            }
    
    type BanOnReplyCommandContext = {
        From: UserUsername
        MessageId: TelegramMessageId
        ReplyToMessageId: TelegramMessageId
        ChatId: TelegramChatId
        UserId: TelegramUserId
        Username: UserUsername option
    }
    
    type BanCommandContext = {
        From: UserUsername
        MessageId: TelegramMessageId
        ChatId: TelegramChatId
        Usernames: UserUsername seq
        Until: DateTime
    }
    
    type UnbanCommandContext = {
        From: UserUsername
        MessageId: TelegramMessageId
        ChatId: TelegramChatId
        Usernames: UserUsername seq
    }
         
    type Command =
        | BanCommand of BanCommandContext
        | BanOnReplyCommand of BanOnReplyCommandContext
        | UnbanCommand of UnbanCommandContext
        | DoNothingCommand
    
    type CommandError =
        | ApiError of string
        | AdminBanNotAllowedError of string
    
    type IMessage =
        abstract member FormatAsString: unit -> string
    
    type BanOnReplyMessage =
        { Username: UserUsername
          UserId: TelegramUserId
          Chats: ChatUsername seq }
        interface IMessage with
            member __.FormatAsString() =
                let chatsText =
                    __.Chats
                    |> Seq.map ^ fun chat -> %chat
                    |> String.join ", "
                        
                sprintf "Banned %i (%s) in chats %s forever" %__.UserId %__.Username chatsText
    
    type BanMessage =
        { Usernames: UserUsername seq
          Chats: ChatUsername seq
          Until: DateTime }
        interface IMessage with
            member __.FormatAsString() =
                let durationText =
                    if __.Until > DateTime.UtcNow.AddYears(1) then
                        "forever"
                    else
                        __.Until.ToString("yyyy-MM-dd HH:mm:ss")
                            
                let usernamesText =
                    __.Usernames
                    |> Seq.map ^ fun username -> %username
                    |> String.join ", "
                    
                let chatsText =
                    __.Chats
                    |> Seq.map ^ fun chat -> %chat
                    |> String.join ", "
                        
                sprintf "Banned %s in chats %s until %s UTC" usernamesText chatsText durationText
                
    type UnbanMessage =
        { Usernames: UserUsername seq
          Chats: ChatUsername seq }
        interface IMessage with
            member __.FormatAsString() =
                let usernamesText =
                    __.Usernames
                    |> Seq.map ^ fun username -> %username
                    |> String.join ", "
                    
                let chatsText =
                    __.Chats
                    |> Seq.map ^ fun chat -> %chat
                    |> String.join ", "
                sprintf "Unbanned %s in chats %s" usernamesText chatsText
    
    type CommandMessage = 
        | BanMessage of UserUsername * BanMessage * CommandError array
        | BanOnReplyMessage of UserUsername * BanOnReplyMessage * CommandError array
        | UnbanMessage of UserUsername * UnbanMessage * CommandError array
        | DoNothingMessage
        
    let prepareReplyToMessage (botUsername: string option) (reply: ReplyMessage) =
        botUsername
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
            //if someone added user
            if reply.ReplyToMessage.From = reply.Message.From then
                reply.ReplyToMessage.NewChatMember
                |> Option.map ^ fun user ->
                     { BotUsername = %botUsername
                       Message = message
                       MessageText = text
                       ReplyToUser = user
                       ReplyToMessage = reply.ReplyToMessage
                       FromUsername = %username
                       ChatUsername = %(sprintf "@%s" chatUsername) }
            else
                match reply.ReplyToMessage.From with
                | Some from -> 
                  { BotUsername = %botUsername
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
                        { BotUsername = %botUsername
                          Message = message
                          MessageText = text
                          ReplyToUser = user
                          ReplyToMessage = reply.ReplyToMessage
                          FromUsername = %username
                          ChatUsername = %(sprintf "@%s" chatUsername) }

    let parseReplyMessage (botSettings: BotSettings) (context: ReplyToMessageContext) =
        match isCommandAllowed botSettings context.FromUsername context.ChatUsername with
        | CommandAllowed ->
            if context.MessageText.Contains("ban") && context.MessageText.Contains(%context.BotUsername) then
                let context = {
                    From = %context.FromUsername
                    MessageId = %context.Message.MessageId
                    ReplyToMessageId = %context.ReplyToMessage.MessageId
                    ChatId = %context.Message.Chat.Id
                    UserId = %context.ReplyToUser.Id
                    Username = context.ReplyToUser.Username |> Option.map ^ fun username -> %username
                }
                BanOnReplyCommand context
            else
                DoNothingCommand
        | CommandNotAllowed ->
            DoNothingCommand
        
    let parseTextMessage (botSettings: BotSettings) (context: UserTextMessageContext): Command =
        match isCommandAllowed botSettings context.FromUsername context.ChatUsername with
        | CommandAllowed ->
            match Parser.parse %context.BotUsername context.MessageText with
            | BotCommand(Command((Usernames usernames), Ban(duration))) ->
                let until =
                    match duration with
                    | Forever ->
                        DateTime.UtcNow.AddMonths(13)
                    | Timed date ->
                        date
                let usernames =
                    usernames
                    |> Seq.map ^ fun username -> %username
                
                let context = {
                    From = %context.FromUsername
                    MessageId = %context.Message.MessageId
                    ChatId = %context.Message.Chat.Id
                    Usernames = usernames
                    Until = until
                }
                BanCommand context
            | BotCommand(Command((Usernames usernames), Unban)) ->
                let usernames =
                    usernames
                    |> Seq.map ^ fun username -> %username
                    
                let context = {
                    From = %context.FromUsername
                    MessageId = %context.Message.MessageId
                    ChatId = %context.Message.Chat.Id
                    Usernames = usernames
                }
                UnbanCommand context
            | InvalidCommand _ ->
                DoNothingCommand
        | CommandNotAllowed ->
            DoNothingCommand
                
    let executeCommand (botSettings: BotSettings) (botApi: IBotApi) (dataApi: IDataAccessApi) command = async {
        let getErrors results =
            results
            |> Result.partition
            |> snd
        
        let createCommandError fn text: Result<unit, CommandError> =
            text |> (fn >> Result.Error)
            
        let userCanBeBanned username =
            botSettings.AllowedUsers.Set
            |> Set.contains username
            |> not
        
        match command with
        | BanCommand context ->
            do! botApi.DeleteMessage context.ChatId context.MessageId
            
            let requests =
                [for user in context.Usernames do
                    if userCanBeBanned user then
                        for chat in botSettings.ChatsToMonitor.Set do
                            yield botApi.RestrictUser chat user context.Until
                                  |> Async.Map ^ fun result ->
                                      Result.mapError ApiError result
                    else
                        yield sprintf "Cannot ban admin @%s" %user
                              |> createCommandError AdminBanNotAllowedError
                              |> async.Return]
                
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
                    
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Usernames = context.Usernames
                Until = context.Until
            }
            
            return BanMessage(context.From, message, errors)
        | BanOnReplyCommand context ->
            let! username = async {
                match context.Username with
                | Some username ->
                    do! dataApi.UpsertUsers [DataAccess.User(UserId = %context.UserId, Username = %username)]
                    return username
                | None ->
                    return! dataApi.GetUsernameByUserId context.UserId
                            |> Async.Map (Option.defaultValue %"unknown user")
            }
                
            do! botApi.DeleteMessage context.ChatId context.MessageId
            
            let requests =
                if userCanBeBanned username then
                    [yield botApi.DeleteMessage context.ChatId context.ReplyToMessageId
                           |> Async.Map Ok
                     for chat in botSettings.ChatsToMonitor.Set do
                        yield botApi.RestrictUserById chat context.UserId (DateTime.UtcNow.AddMonths(13))
                              |> Async.Map ^ fun result ->
                                    Result.mapError ApiError result]
                else
                    [sprintf "Cannot ban admin %s" %username
                     |> createCommandError AdminBanNotAllowedError
                     |> async.Return]
                    
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
            
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Username = username
                UserId = context.UserId
            }
            
            return BanOnReplyMessage(context.From, message, errors)
        | UnbanCommand context ->
            do! botApi.DeleteMessage context.ChatId context.MessageId
            
            let requests =
                [for user in context.Usernames do
                    for chat in botSettings.ChatsToMonitor.Set do
                        yield botApi.UnrestrictUser chat user
                              |> Async.Map ^ fun result ->
                                  Result.mapError ApiError result]
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
            
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Usernames = context.Usernames
            }
            
            return UnbanMessage(context.From, message, errors)
        | DoNothingCommand ->
            return DoNothingMessage
    }
    
    let parseAndExecuteTextMessage settings botApi dataApi message =
        parseTextMessage settings message
        |> executeCommand settings botApi dataApi
    
    let parseAndExecuteReplyMessage settings botApi dataApi message =
        parseReplyMessage settings message
        |> executeCommand settings botApi dataApi
        
    let processAdminCommand (botSettings: BotSettings) (config: BotConfig) fileId = async {
        match! ApiExt.prepareAndDownloadFile config fileId with
        | Ok stream ->
            let users = JsonNet.deserializeFromStream<DataAccess.User[]>(stream)
            do! Datastore.upsertUsers users
            do! "Updated user database"
                |> ApiExt.sendMessage botSettings.ChannelId config
        | Error e ->
            do! ApiExt.sendMessage botSettings.ChannelId config e
    }
    
    let processNewUsersCommand (users: Types.User seq) =
        users
        |> Seq.filter ^ fun u -> Option.isSome u.Username
        |> Seq.map ^ fun u ->
            DataAccess.User(UserId = u.Id, Username = Option.get u.Username)
        |> Datastore.upsertUsers
        
    module Logging =
        let concatErrors (errors: CommandError seq) =
            [for error in errors do
                match error with
                | ApiError e -> yield e
                | AdminBanNotAllowedError e -> yield e]
            |> String.join "\n"
            
        let formatHeader commandName username (message: IMessage) =
            sprintf "%s command from: @%s\n\n%s" commandName %username (message.FormatAsString())
            
        let logСommandToChannel (botApi: IBotApi) commandMessage = async {
            match commandMessage with
            | BanMessage(fromUsername, message, errors) ->
                do! sprintf "%s\n\n%s" (formatHeader "Ban" fromUsername message) (concatErrors errors)
                    |> botApi.SendTextToChannel
            | UnbanMessage(fromUsername, message, errors) ->
                do! sprintf "%s\n\n%s" (formatHeader "Unban" fromUsername message) (concatErrors errors)
                    |> botApi.SendTextToChannel
            | BanOnReplyMessage(fromUsername, message, errors) ->
                do! sprintf "%s\n\n%s" (formatHeader "Ban on reply" fromUsername message) (concatErrors errors)
                    |> botApi.SendTextToChannel
            | DoNothingMessage -> ()
        }
        