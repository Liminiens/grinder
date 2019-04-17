namespace Grinder

open Grinder
open Grinder.Funogram
open Funogram.Api
open Funogram.Bot
open Funogram.Types
open System

type BotSettings = {
    ChatsToMonitor: string array
    AllowedUsers: string array
    Channel: int64
}
    
module Control =
    open FSharp.Text.RegexProvider
    
    type CommandType =
        | Restrict of string
        | UnRestrict of string
    
    module CommandType =
        let [<Literal>] banText = "ban"
        let [<Literal>] unbanText = "unban"
            
        let parse (text: string) =
            if text.StartsWith(banText) then
                text.Substring(banText.Length, text.Length - banText.Length).Trim()
                |> Restrict
                |> Some
            elif text.StartsWith(unbanText) then
                text.Substring(unbanText.Length, text.Length - unbanText.Length).Trim()
                |> UnRestrict
                |> Some
            else
                None
    
    type Command = {
        Usernames: string list
        Text: CommandType
    }

    type MessageToBotValidationResult = 
        | ValidMessage of string
        | InvalidMessage
            
    type CommandParsingResult =
        | Command of Command
        | InvalidCommand
    
    let validate (botUsername: string) (text: string) = 
        let text = text.Trim()
        if text.StartsWith(botUsername) then
            let message = text.Substring(0, botUsername.Length - 1).Trim()
            ValidMessage message
        else
            InvalidMessage
   
    type MinutesRegex = Regex< "(?<value>\d+)\s+min(s)?" >
    
    type DaysRegex = Regex< "(?<value>\d+)\s+day(s)?" >
    
    type MonthsRegex = Regex< "(?<value>\d+)\s+month(s)?" >

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
                Command { Usernames = usernames
                          Text = command }
            | None ->
                InvalidCommand

    let parse botUsername text =
        match validate botUsername text with
        | ValidMessage text -> 
            match getUsernamesAndCommand text with
            | Command data ->
                let minutes = Minutes.Parse text
                let days = Days.Parse text
                let months = Months.Parse text
                ()
            | InvalidCommand -> ()   
        | InvalidMessage -> ()

module Processing =
    type BotMessage = 
        | Message of Message

    [<RequireQualifiedAccess>]
    module BotMessage =
        let fromUpdate (update: Update) =
            update.Message
            |> Option.bind ^ fun message ->
                Message message |> Some
    
    let onUpdate (settings: BotSettings) (context: UpdateContext) =
        let isAllowedUser username =
            settings.AllowedUsers 
            |> Array.contains username

        let isAllowedChat chatUsername =
            settings.ChatsToMonitor 
            |> Array.contains chatUsername
        
        let restrictUser context chat userId until =
            async {
                let! user = 
                    getChatMemberByChatName chat userId
                    |> callApiWithRetry context 4
                match user with
                | Ok _ ->
                    do! 
                        restrictChatMemberBase (Funogram.Types.String(chat)) userId (Some until) (Some false) (Some false) (Some false) (Some false)
                        |> callApiWithRetry context 4
                        |> Async.Ignore      
                    return sprintf "Restricted in %s" chat
                | Error _ ->
                    return sprintf "Not restricted in %s" chat
            }
        
        let handleMessage (message: Message) =
            async {
                return!
                    context.Me.Username
                    |> Option.bind ^ fun botUsername ->
                        message.From
                        |> Option.map ^ fun from ->
                            (botUsername, message, from)
                    |> Option.bind ^ fun (botUsername, message, from) ->
                        from.Username
                        |> Option.bind ^ fun username ->
                            if isAllowedUser username then 
                                Some (botUsername, message, from, username)
                            else 
                                None
                    |> Option.bind ^ fun (botUsername, message, from, username) ->
                        message.Chat.Username
                        |> Option.bind ^ fun username ->
                            if isAllowedChat username then 
                                Some (botUsername, message, from, username)
                            else 
                                None
                    |> Option.map ^ fun (botUsername, message, from, username) ->
                        async {
                            do! deleteMessage message.Chat.Id message.MessageId
                                |> callApiWithRetry context 4
                                |> Async.Ignore
                            let requests = [
                                for chat in settings.ChatsToMonitor do
                                    let time = DateTime.UtcNow.AddMinutes(1.)
                                    yield restrictUser context chat from.Id time
                            ]
                            let! text = Async.Parallel requests
                            ()
                        }
                    |> Option.defaultValue Async.Unit
            }

        async {
            do! BotMessage.fromUpdate context.Update
                |> Option.map ^ fun botMessage ->
                    async {
                        match botMessage with
                        | Message message -> 
                            do! handleMessage message
                    }
                |> Option.defaultValue Async.Unit
        } |> Async.Start


module Program =
    open Processing
    open System.Threading.Tasks
    open System.Threading  
    open System.Net.Http
    open System.IO
    open MihaZupan
    open Newtonsoft.Json
    
    [<CLIMutable>]
    type Socks5Configuration = {
        Hostname: string
        Port: int
        Username: string
        Password: string
    }

    [<CLIMutable>]
    type BotConfig = {
        Socks5Proxy: Socks5Configuration
        Token: string
        ChatsToMonitor: string array
        AllowedUsers: string array
        Channel: int64
    }
    
    let createHttpClient config =
        let messageHandler = new HttpClientHandler()
        messageHandler.Proxy <- HttpToSocks5Proxy(config.Hostname, config.Port, config.Username, config.Password)
        messageHandler.UseProxy <- true
        new HttpClient(messageHandler)
    
    [<EntryPoint>]
    let main _ =
        let config =
            File.ReadAllText(Path.Combine(Directory.GetCurrentDirectory(), "bot_config.json"))
            |> JsonConvert.DeserializeObject<BotConfig>

        let botConfiguration = { 
            defaultConfig with 
                Token = config.Token
                Client = createHttpClient config.Socks5Proxy
                AllowedUpdates = ["message"] |> Seq.ofList |> Some
        }

        async {
            printfn "Starting bot"
            let settings = 
                { ChatsToMonitor = config.ChatsToMonitor
                  AllowedUsers = config.AllowedUsers
                  Channel = config.Channel }
            do! startBot botConfiguration (onUpdate settings) None
                |> Async.StartChild
                |> Async.Ignore
            printfn "Bot started"
            do! Task.Delay(Timeout.InfiniteTimeSpan) |> Async.AwaitTask
        } |> Async.RunSynchronously
        
        printfn "Bot exited"
        0 // return an integer exit code