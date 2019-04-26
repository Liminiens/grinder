namespace Grinder.Commands

open System
open Grinder

module Control =
    open FSharp.Text.RegexProvider
    
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
        if text.StartsWith(botUsername) then
            let message = text.Substring(botUsername.Length, text.Length - botUsername.Length).Trim()
            ValidMessage message
        else
            InvalidMessage
   
    type MinutesRegex = Regex< "(?<value>\d+)\s+min(s|utes)?" >
    
    type DaysRegex = Regex< "(?<value>\d+)\s+day(s)?" >
    
    type MonthsRegex = Regex< "(?<value>\d+)\s+month(s)?" >
    
    type NegativeNumberRegex = Regex< "-\d+" >

    type Minutes =
        private Minutes of int32
            static member Parse(text) =
                let result = MinutesRegex().TypedMatch(text)
                if result.value.Success then
                    let value = int32 result.value.Value
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
                let result = DaysRegex().TypedMatch(text)
                if result.value.Success then
                    let value = int32 result.value.Value
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
                let result = MonthsRegex().TypedMatch(text)
                if result.value.Success then
                    let value = int32 result.value.Value
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
                Command { Usernames = UsernameList usernames
                          Text = command }
            | None ->
                InvalidCommand

    let parseBanCommand usernames time =
        if not <| NegativeNumberRegex().TypedMatch(time).Success then
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
    open Control
    
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
        match (botSettings, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            do! Api.deleteMessage context.Message.Chat.Id context.Message.MessageId
                |> callApiWithDefaultRetry context.UpdateContext
                |> Async.Ignore
            
            let parsedMessage = Control.parse context.BotUsername context.MessageText
            let requests = [
                for chat in botSettings.ChatsToMonitor do
                    match parsedMessage with
                    | Ban(UsernameList usernames, time) ->
                        yield!
                            usernames
                            |> Seq.map ^ fun user ->
                                ApiExt.restrictUser context.UpdateContext chat user time
                    | Unban(UsernameList usernames) ->
                        yield!
                            usernames
                            |> Seq.map ^ fun user ->
                                ApiExt.unrestrictUser context.UpdateContext chat user
                    | IgnoreCommand ->
                        ()
            ]
            let! text = Async.Parallel requests
            do! String.Join('\n', text)
                |> sprintf "Username: %s\n\n%s" context.FromUsername
                |> ApiExt.sendMessage botSettings.Channel context.UpdateContext
        | CommandNotAllowed ->
            ()        
    }