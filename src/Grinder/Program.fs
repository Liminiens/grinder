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

module Processing =
    type BotMessage = 
        | Message of Message
        
    let restrictUser context chat userId until =
        async {
            let! userResponse = 
                getChatMemberByChatName chat userId
                |> callApiWithRetry context 4
            match userResponse with
            | Ok _ ->
                do! 
                    restrictChatMemberBase (Funogram.Types.String(chat)) userId (Some until) (Some false) (Some false) (Some false) (Some false)
                    |> callApiWithRetry context 4
                    |> Async.Ignore      
                return sprintf "Restricted in %s" chat
            | Error _ ->
                return sprintf "Not restricted in %s" chat
        }
            
    [<RequireQualifiedAccess>]
    module BotMessage =
        let fromUpdate (update: Update) =
            update.Message
            |> Option.map ^ fun message ->
                Message message
    
    let onUpdate (settings: BotSettings) (context: UpdateContext) =
        let isAllowedUser username =
            settings.AllowedUsers 
            |> Array.contains username

        let isAllowedChat chatUsername =
            settings.ChatsToMonitor 
            |> Array.contains chatUsername
        
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
                        |> Option.map ^ fun username ->
                            (botUsername, message, from, username)
                    |> Option.bind ^ fun (botUsername, message, from, username) ->
                        message.Chat.Username
                        |> Option.bind ^ fun chatName ->
                            if isAllowedChat chatName then 
                                Some (botUsername, message, from, username)
                            else 
                                None
                    |> Option.map ^ fun (botUsername, message, from, username) ->
                        async {
                            do! deleteMessage message.Chat.Id message.MessageId
                                |> callApiWithRetry context 4
                                |> Async.Ignore
                            if isAllowedUser username then
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