namespace Grinder.Commands

open System
open Grinder

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
        BotUsername: string
        Message: Message
        MessageText: string
        From: User
        FromUsername: string
        ChatUsername: string
    }
    
    let iterTextMessage fn (context: UpdateContext) (message: Message) =
        context.Me.Username
        |> Option.bind ^ fun botUsername ->
            message.From
            |> Option.map ^ fun from ->
                (botUsername, message, from)
        |> Option.bind ^ fun (botUsername, message, from) ->
            from.Username
            |> Option.map ^ fun username ->
                (botUsername, message, from, username)
        |> Option.bind ^ fun (botUsername, message, from, username) ->
            message.Text
            |> Option.map ^ fun text ->
                (botUsername, message, from, username, text)
        |> Option.bind ^ fun (botUsername, message, from, username, text) ->
            message.Chat.Username
            |> Option.map ^ fun chatUsername -> {
                UpdateContext = context
                BotUsername = botUsername
                Message = message
                MessageText = text
                From = from
                FromUsername = username
                ChatUsername = chatUsername
            }
        |> Option.map fn
        |> Option.defaultValue Async.Unit
    
    let private (|CommandAllowed|CommandNotAllowed|) (botSettings: BotSettings, username, chatUsername) =
        let isAllowedUser username =
            botSettings.AllowedUsers 
            |> Array.contains username

        let isAllowedChat chatUsername =
            botSettings.ChatsToMonitor 
            |> Array.contains chatUsername
            
        if isAllowedUser username && isAllowedChat chatUsername then    
            CommandAllowed
        else
            CommandNotAllowed
            
    let processTextCommand (botSettings: BotSettings) (context: UserMessageContext) = async {
        
        let sendCommandResultToChannel (requestsText: string seq) =
            String.Join('\n', requestsText)
            |> sprintf "Command from: %s\n\n%s" context.FromUsername
            |> ApiExt.sendMessage botSettings.ChannelId context.UpdateContext
        
        let deleteMessageToBot() =
            Api.deleteMessage context.Message.Chat.Id context.Message.MessageId
            |> callApiWithDefaultRetry context.UpdateContext
            |> Async.Ignore
        
        match (botSettings, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            let parsedMessage = Parsing.parse context.BotUsername context.MessageText
            match parsedMessage with
            | Ban(UsernameList usernames, time) ->
                do! deleteMessageToBot()
                
                let! requestsText = 
                    [for chat in botSettings.ChatsToMonitor do
                        for user in usernames do
                            yield ApiExt.restrictUser context.UpdateContext chat user time]
                    |> Async.Parallel
                do! sendCommandResultToChannel requestsText
            | Unban(UsernameList usernames) ->
                do! deleteMessageToBot()
                
                let! requestsText = 
                    [for chat in botSettings.ChatsToMonitor do
                        for user in usernames do
                            yield ApiExt.unrestrictUser context.UpdateContext chat user]
                    |> Async.Parallel
                do! sendCommandResultToChannel requestsText
            | IgnoreCommand -> ()
        | CommandNotAllowed -> ()
    }
    
    let iterAdminCommand (botSettings: BotSettings) (context: UpdateContext) fileId = async {
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
    
    let iterNewUsersCommand (users: Types.User seq) =
        users
        |> Seq.filter ^ fun u -> Option.isSome u.Username
        |> Seq.map ^ fun u ->
            DataAccess.User(UserId = u.Id, Username = Option.get u.Username)
        |> Datastore.upsertUsers
        