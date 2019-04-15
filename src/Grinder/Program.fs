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

[<RequireQualifiedAccess>]
module Caching = 
    open Polly
    open Microsoft.Extensions.Caching.Memory
    open Polly.Caching.Memory

    let private cachePolicy = 
        let provider = new MemoryCacheProvider(new MemoryCache(new MemoryCacheOptions()))
        Policy.CacheAsync(provider, TimeSpan.FromHours(24.0))
    
    let cache key computation = 
        async {
            let! result = 
                cachePolicy.ExecuteAsync<'T>(fun _ -> Async.StartAsTask computation
                                            ,new Context(key))
                |> Async.AwaitTask
            return result
        }

[<AutoOpen>]
module ApiExtensions =
    open Polly

    type ApiCallResult<'T> = Result<'T, ApiResponseError>

    let callApiWithRetry (call: Async<ApiCallResult<'T>>) =
        Policy.HandleResult<ApiCallResult<'T>>(function | Error _ -> true | Ok _ -> false)
              .RetryForeverAsync()
              .ExecuteAsync(fun _ -> Async.StartAsTask call)
        |> Async.AwaitTask     

    let callApi context = api context.Config >> callApiWithRetry    

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
    ChatsToBanIn: string array
    AllowedUsers: string array
}

type BotSettings = {
    ChatsToBanIn: string array
    AllowedUsers: string array
}

type BotMessage = 
    | Message of Message
    | InlineQuery of InlineQuery

[<RequireQualifiedAccess>]
module BotMessage =
    let fromUpdate (update: Update) =
        match update.InlineQuery with
        | Some query -> InlineQuery query |> Some
        | None ->
            match update.Message with
            | Some message -> Message message |> Some
            | None -> None

let onUpdate (settings: BotSettings) (context: UpdateContext) =
    let isAllowedUser username =
        settings.AllowedUsers 
        |> Array.contains username

    let handleMessage (message: Message) =
        async {
            message.From
            |> Option.bind ^ fun from ->
                {| Message = message; From = from |}
                |> Some
            |> Option.bind ^ fun data ->
                data.From.Username
                |> Option.bind ^ fun username ->
                    if isAllowedUser username then 
                        {| data with Username = username |}
                        |> Some
                    else None
            |> Option.iter ^ fun data ->
            
                deleteMessageByChatName "" data.Message.MessageId
                |> callApi context
        }
    
    let handleInlineQuery (query: InlineQuery) = 
        ()

    async {
        BotMessage.fromUpdate context.Update
        |> Option.iter ^ fun botMessage ->
            match botMessage with
            | InlineQuery query -> 
                handleInlineQuery query
            | Message message -> 
                do! handleMessage message
    } |> Async.StartImmediate
    

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
            AllowedUpdates = ["message"; "inline_query"] |> Seq.ofList |> Some
    }

    async {
        printfn "Starting bot"
        let settings = 
            { ChatsToBanIn = config.ChatsToBanIn
              AllowedUsers = config.AllowedUsers }
        do! startBot botConfiguration (onUpdate settings) None
            |> Async.StartChild
            |> Async.Ignore
        printfn "Bot started"
        do! Task.Delay(Timeout.InfiniteTimeSpan) |> Async.AwaitTask
    } |> Async.RunSynchronously
    
    printfn "Bot exited"
    0 // return an integer exit code