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
        member this.Value =
            match this with
            | Forever ->
                DateTime.UtcNow.AddMonths(13)
            | Timed date ->
                date
                
        member this.FormattedStringValue =
            this.Value.ToString("yyyy-MM-dd HH:mm:ss")
                
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

    type Command =
        | UserCommand of Usernames * CommandAction
        | Ping

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
    
    let pban: Parser<CommandAction, unit> =
        str "ban" .>> spaces >>. (pforeverBan <|> pdistinctTimeFractions)
        |>> Ban

    let punban: Parser<CommandAction, unit> =
        str "unban" .>> spaces >>% Unban
        
    let pping: Parser<Command, unit> =
        str "ping" >>% Ping
        
    let pcommandAction: Parser<CommandAction, unit> =
        pban <|> punban

    let parseCommand botUsername =
        pbotUsername botUsername >>.
        (pping <|> pipe2 many1Usernames pcommandAction (fun usernames command -> UserCommand(Usernames(usernames), command)))
        
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
    open Funogram.Types
    open Grinder.FunogramExt
    open Parser
    
    type TextMessageContext = {
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
    
    type AuthorizationResult =
        | CommandAllowed
        | CommandNotAllowed
    
    let authorize (botSettings: BotSettings) user chat =
        let isAllowedUser username =
            botSettings.AllowedUsers.Set
            |> Set.contains username

        let isAllowedChat chatUsername =
            botSettings.ChatsToMonitor.Set
            |> Set.contains chatUsername
            
        if isAllowedUser user && isAllowedChat chat then    
            CommandAllowed
        else
            CommandNotAllowed
    
    let prepareTextMessage (botUsername: string option) (message: Message): TextMessageContext option =
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
                BotUsername = %(sprintf "@%s" botUsername)
                Message = message
                MessageText = text
                FromUsername = %username
                ChatUsername = %(sprintf "@%s" chatUsername)
            }
    
    let prepareReplyToMessage (botUsername: string option) (reply: ReplyMessage): ReplyToMessageContext option =
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
                     { BotUsername = %(sprintf "@%s" botUsername)
                       Message = message
                       MessageText = text
                       ReplyToUser = user
                       ReplyToMessage = reply.ReplyToMessage
                       FromUsername = %username
                       ChatUsername = %(sprintf "@%s" chatUsername) }
            else
                match reply.ReplyToMessage.From with
                | Some from -> 
                  { BotUsername = %(sprintf "@%s" botUsername)
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
                        { BotUsername = %(sprintf "@%s" botUsername)
                          Message = message
                          MessageText = text
                          ReplyToUser = user
                          ReplyToMessage = reply.ReplyToMessage
                          FromUsername = %username
                          ChatUsername = %(sprintf "@%s" chatUsername) }
                        
    type ActionOnReplyCommandContext = {
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
        Until: BanDuration
    }
    
    type UnbanCommandContext = {
        From: UserUsername
        MessageId: TelegramMessageId
        ChatId: TelegramChatId
        Usernames: UserUsername seq
    }

    type PingContext = {
        ChatId: TelegramChatId
    }

    type Command =
        | BanCommand of BanCommandContext
        | BanOnReplyCommand of ActionOnReplyCommandContext
        | UnbanCommand of UnbanCommandContext
        | UnbanOnReplyCommand of ActionOnReplyCommandContext
        | PingCommand of PingContext
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
          Until: BanDuration }
        interface IMessage with
            member __.FormatAsString() =
                let durationText =
                    if __.Until.Value > DateTime.UtcNow.AddYears(1) then
                        "forever"
                    else
                        sprintf "until %s UTC" __.Until.FormattedStringValue
                        
                let usernamesText =
                    __.Usernames
                    |> Seq.map ^ fun username -> %username
                    |> String.join ", "
                    
                let chatsText =
                    __.Chats
                    |> Seq.map ^ fun chat -> %chat
                    |> String.join ", "
                        
                sprintf "Banned %s in chats %s %s" usernamesText chatsText durationText
                
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
        | UnbanOnReplyMessage of UserUsername * UnbanMessage * CommandError array
        
    let parseReplyMessage (context: ReplyToMessageContext): Command =
        let messageText = context.MessageText
        let botMentioned = messageText.StartsWith(%context.BotUsername)
        
        // bot nickname and command are delimited by space
        let hasUnbanWord = messageText.Contains(" unban")
        let hasBanWord = messageText.Contains(" ban") 
        
        let context = {
                From = %context.FromUsername
                MessageId = %context.Message.MessageId
                ReplyToMessageId = %context.ReplyToMessage.MessageId
                ChatId = %context.Message.Chat.Id
                UserId = %context.ReplyToUser.Id
                Username = context.ReplyToUser.Username
                           |> Option.map ^ fun username -> %(sprintf "@%s" username)
            }
                
        match (botMentioned, hasBanWord, hasUnbanWord) with
            | (true, true, false) -> BanOnReplyCommand context
            | (true, false, true) -> UnbanOnReplyCommand context
            | _ -> DoNothingCommand
                            
    let parseTextMessage (context: TextMessageContext): Command =
        match Parser.parse %context.BotUsername context.MessageText with
        | BotCommand(UserCommand((Usernames usernames), Ban(duration))) ->
            let usernames =
                usernames
                |> Seq.map ^ fun username -> %username
            
            let context = {
                From = %context.FromUsername
                MessageId = %context.Message.MessageId
                ChatId = %context.Message.Chat.Id
                Usernames = usernames
                Until = duration
            }
            BanCommand context
        | BotCommand(UserCommand((Usernames usernames), Unban)) ->
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
        | BotCommand(Ping) ->
            PingCommand { ChatId = %context.Message.Chat.Id }
        | InvalidCommand _ ->
            DoNothingCommand
                
    let executeCommand (botSettings: BotSettings) (botApi: IBotApi) (dataApi: IDataAccessApi) command: Async<CommandMessage option> = async {
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
            
        let logErrors =
            Seq.iter (function
                | ApiError str
                | AdminBanNotAllowedError str -> logErr str
            )
        
        match command with
        | BanCommand context ->
            do! botApi.DeleteMessage context.ChatId context.MessageId
            
            let requests =
                [for user in context.Usernames do
                    if userCanBeBanned user then
                        for chat in botSettings.ChatsToMonitor.Set do
                            yield botApi.BanUserByUsername chat user context.Until.Value
                                  |> Async.Map ^ Result.mapError
                                          (sprintf "Error on baning user %A in chat %A until %A. %s"
                                               user
                                               chat
                                               context.Until.Value
                                           >> ApiError)
                    else
                        yield sprintf "Cannot ban admin @%s" %user
                              |> createCommandError AdminBanNotAllowedError
                              |> async.Return]
                
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
                
            logErrors errors
                    
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Usernames = context.Usernames
                Until = context.Until
            }
            
            return Some <| BanMessage(context.From, message, errors)
        | BanOnReplyCommand context ->
            let! username = async {
                match context.Username with
                | Some username ->
                    do! dataApi.UpsertUsers [DataAccess.User(UserId = %context.UserId, Username = %username)]
                    return username
                | None ->
                    let! username = dataApi.GetUsernameByUserId context.UserId
                    return username
                           |> Option.map ^ fun name -> %(sprintf "@%s" %name)
                           |> Option.defaultValue %"unknown user"
            }
                
            do! botApi.DeleteMessage context.ChatId context.MessageId
            do! botApi.DeleteMessage context.ChatId context.ReplyToMessageId
            
            let requests =
                if userCanBeBanned username then
                    [for chat in botSettings.ChatsToMonitor.Set do
                        let until = DateTime.UtcNow.AddMonths(13)
                        yield botApi.BanUserByUserId chat context.UserId until
                              |> Async.Map ^ Result.mapError
                                          (sprintf "Error on baning userId %A in chat %A until %A. %s"
                                               context.UserId
                                               chat
                                               until
                                           >> ApiError)]
                else
                    [sprintf "Cannot ban admin %s" %username
                     |> createCommandError AdminBanNotAllowedError
                     |> async.Return]
                    
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
                
            logErrors errors
            
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Username = username
                UserId = context.UserId
            }
            
            return Some <| BanOnReplyMessage(context.From, message, errors)
        | UnbanCommand context ->
            do! botApi.DeleteMessage context.ChatId context.MessageId
            
            let requests =
                [for user in context.Usernames do
                    for chat in botSettings.ChatsToMonitor.Set do
                        yield botApi.UnbanUser chat user
                              |> Async.Map ^ Result.mapError
                                          (sprintf "Error on unbaning user %A in chat %A. %s"
                                               user
                                               chat
                                           >> ApiError)
                        yield botApi.UnrestrictUser chat user
                              |> Async.Map ^ Result.mapError
                                          (sprintf "Error on unrestricting user %A in chat %A. %s"
                                               user
                                               chat
                                           >> ApiError)]
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
            
            logErrors errors
            
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Usernames = context.Usernames
            }
            
            return Some <| UnbanMessage(context.From, message, errors)
        | UnbanOnReplyCommand context ->
            let! username = async {
                match context.Username with
                | Some username ->
                    do! dataApi.UpsertUsers [DataAccess.User(UserId = %context.UserId, Username = %username)]
                    return username
                | None ->
                    let! username = dataApi.GetUsernameByUserId context.UserId
                    return username
                           |> Option.map ^ fun name -> %(sprintf "@%s" %name)
                           |> Option.defaultValue %"unknown user"
            }
            
            do! botApi.DeleteMessage context.ChatId context.MessageId
            
            let requests =
                [for chat in botSettings.ChatsToMonitor.Set do
                    yield botApi.UnbanUser chat username
                    |> Async.Map ^ Result.mapError
                          (sprintf "Error on unban user %A in chat %A. %s"
                               username
                               chat
                           >> ApiError)]
                    
            let! errors =
                requests
                |> Async.Parallel
                |> Async.Map getErrors
            
            logErrors errors
            
            let message = {
                Chats = botSettings.ChatsToMonitor.Set
                Usernames = Set.empty.Add username
            }
            
            return Some <| UnbanOnReplyMessage(context.From, message, errors)
        | PingCommand context ->
            do! botApi.SendTextMessage context.ChatId "pong"
            return None
        | DoNothingCommand ->
            logDbg "Do Nothing Command has been successfully processed and we haven't done anything remotely useful"
            return None
    }
    
    let parseAndExecuteTextMessage settings botApi dataApi message: Async<CommandMessage option> =
        parseTextMessage message
        |> executeCommand settings botApi dataApi
    
    let parseAndExecuteReplyMessage settings botApi dataApi message: Async<CommandMessage option> =
        parseReplyMessage message
        |> executeCommand settings botApi dataApi
        
    let processAdminCommand (botSettings: BotSettings) (config: BotConfig) fileId: Async<unit> = async {
        match! ApiExt.prepareAndDownloadFile config fileId with
        | Ok stream ->
            let users = JsonNet.deserializeFromStream<DataAccess.User[]>(stream)
            do! Datastore.upsertUsers users
            do! "Updated user database"
                |> ApiExt.sendMessage botSettings.ChannelId config
            sprintf "Successfully processed admin command for fileId %s" fileId
            |> logInfo
            
        | Error e ->
            sprintf "Error on processing admin command with fileId: %s. Error: %s" fileId e
            |> logErr
            do! ApiExt.sendMessage botSettings.ChannelId config e
    }
    
    let processNewUsersCommand (users: Types.User seq): Async<unit> =
        users
        |> Seq.filter ^ fun u -> Option.isSome u.Username
        |> Seq.map ^ fun u ->
            DataAccess.User(UserId = u.Id, Username = Option.get u.Username)
        |> Datastore.upsertUsers
        
    let formatMessage: CommandMessage -> string =
        let concatErrors (errors: CommandError seq) =
            let errorsText = 
                [for error in errors do
                    match error with
                    | ApiError e -> yield e
                    | AdminBanNotAllowedError e -> yield e]
            match errorsText with
            | [] ->
                String.Empty
            | text ->
                text
                |> String.join "\n"
                |> sprintf "\n\n%s"
        
        let formatHeader commandName username (message: IMessage) =
            sprintf "%s command from: @%s\n\n%s" commandName %username (message.FormatAsString())
            
        function
        | BanMessage(fromUsername, message, errors) ->
            sprintf "%s%s" (formatHeader "Ban" fromUsername message) (concatErrors errors)
        | UnbanMessage(fromUsername, message, errors) ->
            sprintf "%s%s" (formatHeader "Unban" fromUsername message) (concatErrors errors)
        | BanOnReplyMessage(fromUsername, message, errors) ->
            sprintf "%s%s" (formatHeader "Ban on reply" fromUsername message) (concatErrors errors)
        | UnbanOnReplyMessage (fromUsername, message, errors) ->
            sprintf "%s%s" (formatHeader "Unban on reply" fromUsername message) (concatErrors errors)
