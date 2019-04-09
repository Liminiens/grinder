open System
open Funogram.Api
open Funogram.Bot
open Funogram.Types
open Microsoft.Extensions.Caching.Memory
open System.Net.Http
open MihaZupan
open Newtonsoft.Json
open Polly
open System.IO
open System.Threading.Tasks
open System.Threading
open Polly.Caching.Memory

[<AutoOpen>]
module Operators =
    let (^) f x = f x

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
    ChatsToBanIn: int64 array
}

type BotSettings = {
    ChatsToBanIn: int64 array
}

let cache :  string -> Async<'T> -> Async<'T> = 
    let provider = new MemoryCacheProvider(new MemoryCache(new MemoryCacheOptions()))
    let cachePolicy = Policy.CacheAsync(provider, TimeSpan.FromHours(24.0))
    fun key computation -> async {
        let! result = 
            cachePolicy.ExecuteAsync<'T>(fun ctx -> Async.StartAsTask computation
                                        ,new Context(key))
            |> Async.AwaitTask
        return result
    }

type ApiCallResult<'T> = Result<'T, ApiResponseError>

let callApiWithRetry (call: Async<ApiCallResult<'T>>) =
    Policy.HandleResult<ApiCallResult<'T>>(function | Error _ -> true | Ok _ -> false)
          .RetryAsync(5)
          .ExecuteAsync(fun _ -> Async.StartAsTask call)
    |> Async.AwaitTask     

let callApi context = api context.Config >> callApiWithRetry

let getAdministrators context chat = async {
    match! getChatAdministrators chat |> callApi context with
    | Ok data ->
        return data |> List.ofSeq
    | Error _ ->
        return []
}

let getAllAdministrators context chats = async {
    let! admins = 
        chats
        |> Seq.map (getAdministrators context)
        |> Async.Parallel
    return admins
            |> Seq.collect id
            |> Seq.distinct
            |> List.ofSeq
}

let onUpdate (settings: BotSettings) (context: UpdateContext) =
    async {
        let isUserAdmin userId = async {
            let! admins = 
                getAllAdministrators context settings.ChatsToBanIn
                |> cache "Admins"
            return admins
                   |> Seq.exists ^ fun admin -> admin.User.Id = userId
        }

        context.Update.Message
        |> Option.bind ^ fun message ->
            match message.From with
            | Some from -> Some(message, from)
            | None -> None
        |> Option.bind ^ fun (message, from) ->
            let a = isUserAdmin from.Id
            None
        |> Option.iter ^ fun (message, from) ->
            processCommands context [
                cmd "/ban" ^ fun ctx -> ()
                cmd "/unban" ^ fun ctx -> ()
            ]
            |> ignore
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
            AllowedUpdates = ["message"] |> Seq.ofList |> Some
    }

    async {
        printfn "Starting bot"
        do! startBot botConfiguration (onUpdate { ChatsToBanIn = config.ChatsToBanIn }) None
            |> Async.StartChild
            |> Async.Ignore
        printfn "Bot started"
        do! Task.Delay(Timeout.InfiniteTimeSpan) |> Async.AwaitTask
    } |> Async.RunSynchronously
    
    printfn "Bot exited"
    0 // return an integer exit code