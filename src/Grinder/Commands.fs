namespace Grinder.Commands

open System
open Grinder
open Grinder.Types
open FSharp.UMX

module Parsing =
    open System.Text.RegularExpressions
    
    type CommandText =
        | BanCommandText of string
        | UnbanCommandText of string
        
    type UsernameList = UsernameList of string list
    
    type Command =
        | Ban of  UsernameList * DateTime
        | Unban of UsernameList
        | IgnoreCommand
        
    module CommandType =
        let [<Literal>] banText = "ban"
        let [<Literal>] unbanText = "unban"
            
        let parse (text: string) =
            if text.StartsWith(banText) then
                text.Substring(banText.Length, text.Length - banText.Length).Trim()
                |> BanCommandText
                |> Some
            elif text.StartsWith(unbanText) then
                text.Substring(unbanText.Length, text.Length - unbanText.Length).Trim()
                |> UnbanCommandText
                |> Some
            else
                None
    
    type ParsedCommand = {
        Usernames: UsernameList
        Text: CommandText
    }

    type MessageToBotValidationResult = 
        | ValidMessage of string
        | InvalidMessage
            
    type CommandParsingResult =
        | Command of ParsedCommand
        | InvalidCommand
    
    let validate (botUsername: string) (text: string) = 
        let text = text.Trim()
        let botUsername = sprintf "@%s" botUsername
        if text.StartsWith(botUsername) then
            let message = text.Substring(botUsername.Length, text.Length - botUsername.Length).Trim()
            ValidMessage message
        else
            InvalidMessage
   
    let MinutesRegex = new Regex("(?<value>\d+)\s+min(s|utes)?", RegexOptions.Compiled)
    
    let DaysRegex = new Regex("(?<value>\d+)\s+day(s)?", RegexOptions.Compiled)
    
    let MonthsRegex = new Regex("(?<value>\d+)\s+month(s)?", RegexOptions.Compiled)
    
    let NegativeNumberRegex = new Regex("-\d+", RegexOptions.Compiled)

    type Minutes =
        private Minutes of int32
            static member Parse(text) =
                let result = MinutesRegex.Match(text)
                if result.Groups.["value"].Success then
                    let value = int32 result.Groups.["value"].Value
                    if value > 0 then 
                        value |> Minutes |> Some
                    else
                        None
                else
                    None
            
            member __.Value =
                let (Minutes value) = __
                value

    type Days =
        private Days of int32
            static member Parse(text) =
                let result = DaysRegex.Match(text)
                if result.Groups.["value"].Success then
                    let value = int32 result.Groups.["value"].Value
                    if value > 0 then 
                        value |> Days |> Some
                    else
                        None
                else
                    None
            
            member __.Value =
                let (Days value) = __
                value

    type Months =
        private Months of int32
            static member Parse(text) =
                let result = MonthsRegex.Match(text)
                if result.Groups.["value"].Success then
                    let value = int32 result.Groups.["value"].Value
                    if value > 0 then 
                        value |> Months |> Some
                    else
                        None
                else
                    None
            
            member __.Value =
                let (Months value) = __
                value
    
    type Fraction =
        | Minutes of int32
        | Days of int32
        | Months of int32
    
    type Duration() =
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
                value <- value.AddMonths(months)
    
    let getUsernamesAndCommand text =
        let matches = Regex.Matches(text, "@(\w|\d)+")
        let usernames = 
            matches
            |> Seq.map ^ fun m -> m.Value
            |> List.ofSeq
        match List.length usernames with
        | 0 -> 
            InvalidCommand
        | _ ->
            let lastMatch = matches |> Seq.maxBy ^ fun el -> el.Index
            let commandText =
                text.Substring(lastMatch.Index + lastMatch.Length, text.Length - (lastMatch.Index + lastMatch.Length))
                    .Trim()
            match CommandType.parse commandText with
            | Some(command) ->
                let usernames = 
                    usernames
                    |> List.map ^ fun username -> username.TrimStart('@')
                    |> UsernameList
                Command { Usernames = usernames
                          Text = command }
            | None ->
                InvalidCommand

    let parseBanCommand usernames time =
        if not <| NegativeNumberRegex.Match(time).Success then
            let duration = Duration()
            Days.Parse time
            |> Option.iter ^ fun v -> duration.Add(Days v.Value)
            Months.Parse time
            |> Option.iter ^ fun v -> duration.Add(Months v.Value)
            Minutes.Parse time
            |> Option.iter ^ fun v ->
                let value = 
                    if v.Value < 5 && (not <| duration.IsSet()) then 5 else v.Value
                duration.Add(Minutes value)
            if duration.IsSet() then
                Ban (usernames, duration.GetValue())
            else
                IgnoreCommand
        else
            IgnoreCommand
    
    let parse botUsername text =
        match validate botUsername text with
        | ValidMessage text -> 
            match getUsernamesAndCommand text with
            | Command data ->
                match data.Text with
                | BanCommandText commandText ->
                    match commandText with
                    | ""
                    | "forever" ->
                        Ban (data.Usernames, DateTime.UtcNow.AddMonths(13))
                    | time ->
                        parseBanCommand data.Usernames time
                | UnbanCommandText _ ->
                    Unban data.Usernames
            | InvalidCommand ->
                IgnoreCommand  
        | InvalidMessage ->
            IgnoreCommand
            
module Processing =
    open Funogram
    open Funogram.Bot
    open Funogram.Types
    open Grinder.FunogramExt
    open Parsing
    
    type UserMessageContext = {
        UpdateContext: UpdateContext
        BotUsername: UserUsername
        Message: Message
        MessageText: string
        FromUsername: UserUsername
        ChatUsername: ChatUsername
    }
    
    type ReplyToMessageContext = {
        UpdateContext: UpdateContext
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
    
    let private concatErros results =
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
                UpdateContext = context
                BotUsername = %botUsername
                Message = message
                MessageText = text
                FromUsername = %username
                ChatUsername = %chatUsername
            }
        |> Option.map fn
        |> Option.defaultValue Async.Unit
  
    let processTextCommand (botSettings: BotSettings) (context: UserMessageContext) = async {
        match (botSettings, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            let parsedMessage = Parsing.parse %context.BotUsername context.MessageText
            match parsedMessage with
            | Ban(UsernameList usernames, time) ->
                do! deleteMessage context.UpdateContext context.Message.Chat.Id context.Message.MessageId
                
                let! requestsResult = 
                    [for chat in botSettings.ChatsToMonitor.Set do
                        for user in usernames do
                            if userCanBeBanned botSettings user then
                                yield ApiExt.restrictUser context.UpdateContext chat user time
                            else
                                yield async.Return (Result.Error <| sprintf "Cannot ban admin @%s" user) ]
                    |> Async.Parallel
                    
                let message =
                    let durationText =
                        if time > DateTime.UtcNow.AddMonths(12) then
                            "forever"
                        else
                            time.ToString("yyyy-MM-dd HH:mm:ss")
                    let usernamesText =
                        usernames
                        |> List.map (sprintf "@%s")
                        |> String.join ", "
                    let chatsText =
                        botSettings.ChatsToMonitor.Set
                        |> Set.map (sprintf "@%s")
                        |> String.join ", "
                    sprintf "Banned %s in chats %s until %s UTC" usernamesText chatsText durationText
                    
                do! concatErros requestsResult
                    |> sprintf "Command from: @%s\n\n%s\n%s" %context.FromUsername message
                    |> ApiExt.sendMessage botSettings.ChannelId context.UpdateContext
                    
            | Unban(UsernameList usernames) ->
                do! deleteMessage context.UpdateContext context.Message.Chat.Id context.Message.MessageId
                
                let! requestsResult = 
                    [for chat in botSettings.ChatsToMonitor.Set do
                        for user in usernames do
                            yield ApiExt.unrestrictUser context.UpdateContext chat user]
                    |> Async.Parallel
                    
                let message =
                    let usernamesText =
                        usernames
                        |> List.map (sprintf "@%s")
                        |> String.join ", "
                    let chatsText =
                        botSettings.ChatsToMonitor.Set
                        |> Set.map (sprintf "@%s")
                        |> String.join ", "
                    sprintf "Unbanned %s in chats %s" usernamesText chatsText
                    
                do! concatErros requestsResult
                    |> sprintf "Command from: @%s\n\n%s\n%s" %context.FromUsername message
                    |> ApiExt.sendMessage botSettings.ChannelId context.UpdateContext
            | IgnoreCommand -> ()
        | CommandNotAllowed -> ()
    }
    
    let processAdminCommand (botSettings: BotSettings) (context: UpdateContext) fileId = async {
        let! file = 
            Api.getFile fileId 
            |> callApiWithDefaultRetry context

        match file with
        | Ok data ->
            let uri =
                let filePath = Option.get data.FilePath
                sprintf "https://api.telegram.org/file/bot%s/%s" botSettings.Token filePath
            let! stream = botSettings.ProxyClient.GetStreamAsync(uri) |> Async.AwaitTask
            let users = JsonNet.deserializeFromStream<DataAccess.User[]>(stream)
            do! Datastore.upsertUsers users
            do! "Updated user database"
                |> ApiExt.sendMessage botSettings.ChannelId context
        | Error e ->
            do! sprintf "Failed to download file. Description: %s" e.Description
                |> ApiExt.sendMessage botSettings.ChannelId context
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
                     { UpdateContext = context
                       BotUsername = %botUsername
                       Message = message
                       MessageText = text
                       ReplyToUser = user
                       ReplyToMessage = reply.ReplyToMessage
                       FromUsername = %username
                       ChatUsername = %chatUsername }
            else
                match reply.ReplyToMessage.From with
                | Some from -> 
                  { UpdateContext = context
                    BotUsername = %botUsername
                    Message = message
                    MessageText = text
                    ReplyToUser = from
                    ReplyToMessage = reply.ReplyToMessage
                    FromUsername = %username
                    ChatUsername = %chatUsername }
                  |> Some
                | None ->
                   reply.ReplyToMessage.NewChatMember
                   |> Option.map ^ fun user -> 
                        { UpdateContext = context
                          BotUsername = %botUsername
                          Message = message
                          MessageText = text
                          ReplyToUser = user
                          ReplyToMessage = reply.ReplyToMessage
                          FromUsername = %username
                          ChatUsername = %chatUsername }
        |> Option.map fn
        |> Option.defaultValue Async.Unit
    
    let processReplyMessage (botSettings: BotSettings) (context: ReplyToMessageContext) = async {
        let getUsernameByIdForLogging userId = async {
            match! Datastore.findUsernameByUserId userId with
            | UsernameFound username ->
                return sprintf "@%s" username
            | UsernameNotFound ->
                return "unknown"
        }
        do! context.ReplyToUser.Username
            |> Option.map ^ fun username ->
                [DataAccess.User(UserId = context.ReplyToUser.Id, Username = username)]
                |> Datastore.upsertUsers
            |> Option.defaultValue Async.Unit
        match (botSettings, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            if context.MessageText.Contains("ban") && context.MessageText.Contains(%context.BotUsername) then
                do! deleteMessage context.UpdateContext context.Message.Chat.Id context.Message.MessageId
                do! deleteMessage context.UpdateContext context.ReplyToMessage.Chat.Id context.ReplyToMessage.MessageId
                
                let! requestsResult = 
                    let banDuration = DateTime.UtcNow.AddMonths(13)
                    
                    [for chat in botSettings.ChatsToMonitor.Set do
                        yield ApiExt.restrictUserById context.UpdateContext chat context.ReplyToUser.Id banDuration]
                    |> Async.Parallel
                
                let! username = getUsernameByIdForLogging context.ReplyToUser.Id
                
                let message =
                    let chatsText =
                        botSettings.ChatsToMonitor.Set
                        |> Set.map (sprintf "@%s")
                        |> String.join ", "
                    sprintf "Banned %i (%s) in chats %s forever" context.ReplyToUser.Id username chatsText
                    
                do! concatErros requestsResult
                    |> sprintf "Command from: @%s\n\n%s\n%s" %context.FromUsername message
                    |> ApiExt.sendMessage botSettings.ChannelId context.UpdateContext
        | CommandNotAllowed -> ()
    }
    
    let processNewUsersCommand (users: Types.User seq) =
        users
        |> Seq.filter ^ fun u -> Option.isSome u.Username
        |> Seq.map ^ fun u ->
            DataAccess.User(UserId = u.Id, Username = Option.get u.Username)
        |> Datastore.upsertUsers
        