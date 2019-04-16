module Grinder

open System
open Funogram.Api
open Funogram.Bot
open Funogram.Types
open System.Net.Http
open MihaZupan
open Newtonsoft.Json
open System.IO
open System.Threading.Tasks
open System.Threading

[<AutoOpen>]
module Operators =
    let (^) f x = f x

[<AutoOpen>]
module ApiExtensions =
    type ApiCallResult<'T> = Result<'T, ApiResponseError>

    let private jitter = new Random()

    let rec private retry times (call: Async<ApiCallResult<'T>>) = async {
        match! call with 
        | Ok ok -> 
            return Ok ok
        | Error e ->
            printfn "Api call error: %A; ErrorCode: %i" e.Description e.ErrorCode
            if times <> 0 then
                let delay = jitter.Next(0, 3) |> float
                do! Task.Delay(TimeSpan.FromSeconds(1. + delay)) |> Async.AwaitTask
                return! retry (times - 1) call
            else
                return Error e
    }

    let callApiWithRetry context times = api context.Config >> retry times  

[<RequireQualifiedAccess>]
module Async =
    let Unit = async { do () }

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
}

type BotSettings = {
    ChatsToMonitor: string array
    AllowedUsers: string array
}

type BotMessage = 
    | Message of Message

[<RequireQualifiedAccess>]
module BotMessage =
    let fromUpdate (update: Update) =
        match update.Message with
        | Some message -> 
            Message message |> Some
        | None -> 
            None

type MessageToBot =
    | Command of {| Usernames: string list; Command: string |}
    | InvalidCommand
    
module Command =
    open System.Text.RegularExpressions

    type MessageType = 
        | CommandForBot of string
        | Nothing

    let validate (botUsername: string) (text: string) = 
        let text = text.Trim()
        if text.StartsWith(botUsername) then
            let command = text.Substring(0, botUsername.Length - 1).Trim()
            CommandForBot command
        else
            Nothing
   
    type Minutes = Minutes of int32

    type Months = Months of int32

    type Days = Days of int32
    
    let getMinutes text = 
        let result = Regex.Match(text, "(\d+)\s+min(s)?")
        if result.Success then
            let value = int32 result.Groups.[0].Value
            if value > 0 then 
                value |> Minutes |> Some
            else
                None
        else
            None
    
    let getDays text = 
        let result = Regex.Match(text, "(\d+)\s+day(s)?")
        if result.Success then
            let value = int32 result.Groups.[0].Value
            if value > 0 then 
                value |> Days |> Some
            else
                None
        else
            None
    
    let getMonths text = 
        let result = Regex.Match(text, "(\d+)\s+month(s)?")
        if result.Success then
            let value = int32 result.Groups.[0].Value
            if value > 0 then 
                value |> Months |> Some
            else
                None
        else
            None

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
            let commandText = text.Substring(lastMatch.Index + lastMatch.Length, text.Length - (lastMatch.Index + lastMatch.Length))
            Command {| Usernames = usernames;  Command = commandText.Trim() |}

    let parse botUsername text =
        match validate botUsername text with
        | CommandForBot text -> 
            match getUsernamesAndCommand text with
            | Command data ->
                ()
            | InvalidCommand -> ()   
        | Nothing -> ()

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
                    restrictChatMemberBase (String(chat)) userId (Some until) (Some false) (Some false) (Some false) (Some false)
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
                |> Option.map ^ fun username ->
                    {| BotUsername = username |}
                |> Option.bind ^ fun data ->
                    message.From
                    |> Option.map ^ fun from ->
                        {| data with Message = message; From = from |}
                |> Option.bind ^ fun data ->
                    data.From.Username
                    |> Option.bind ^ fun username ->
                        if isAllowedUser username then 
                            {| data with Username = username |}
                            |> Some
                        else 
                            None
                |> Option.bind ^ fun data ->
                    data.Message.Chat.Username
                    |> Option.bind ^ fun username ->
                        if isAllowedChat username then 
                            Some data
                        else 
                            None
                |> Option.map ^ fun data ->
                    async {
                        do! deleteMessage data.Message.Chat.Id data.Message.MessageId
                            |> callApiWithRetry context 4
                            |> Async.Ignore
                        let requests = [
                            for chat in settings.ChatsToMonitor do
                                let time = DateTime.UtcNow.AddMinutes(1.)
                                yield restrictUser context chat data.From.Id time
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
            AllowedUpdates = ["message";] |> Seq.ofList |> Some
    }

    async {
        printfn "Starting bot"
        let settings = 
            { ChatsToMonitor = config.ChatsToMonitor
              AllowedUsers = config.AllowedUsers }
        do! startBot botConfiguration (onUpdate settings) None
            |> Async.StartChild
            |> Async.Ignore
        printfn "Bot started"
        do! Task.Delay(Timeout.InfiniteTimeSpan) |> Async.AwaitTask
    } |> Async.RunSynchronously
    
    printfn "Bot exited"
    0 // return an integer exit code