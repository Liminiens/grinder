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
    open Grinder.Control
    
    type BotMessage = 
        | Message of Message
        
    let restrictUser context chat userId until =
        async {
            let! userResponse =
                getChatMemberByChatName chat userId
                |> callApiWithRetry context 4
            match userResponse with
            | Ok _ ->
                do! restrictChatMemberBase (Funogram.Types.String(chat)) userId (Some until) (Some false) (Some false) (Some false) (Some false)
                    |> callApiWithDefaultRetry context
                    |> Async.Ignore
                let dateText = until.ToString("yyyy-MM-dd")
                return sprintf "Banned in %s until %s UTC" chat dateText
            | Error _ ->
                return sprintf "Not banned\found in %s" chat
        }
        
    let unrestrictUser context chat userId =
        async {
            let! userResponse = 
                getChatMemberByChatName chat userId
                |> callApiWithDefaultRetry context
            match userResponse with
            | Ok _ ->
                let time = DateTime.UtcNow.AddMinutes(float 1) |> Some
                do! restrictChatMemberBase (Funogram.Types.String(chat)) userId time (Some true) (Some true) (Some true) (Some true)
                    |> callApiWithDefaultRetry context
                    |> Async.Ignore      
                return sprintf "Unbanned in %s" chat
            | Error _ ->
                return sprintf "Not unbanned\found in %s" chat
        }
        
    let sendMessage chatId context text =
        async {
            do! sendMessageBase (ChatId.Int chatId) text None None None None None
                |> callApiWithDefaultRetry context
                |> Async.Ignore  
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
        
        let sendMessageToChannel = sendMessage settings.Channel context
        
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
                        message.Text
                        |> Option.map ^ fun text ->
                            async {
                                do! deleteMessage message.Chat.Id message.MessageId
                                    |> callApiWithRetry context 4
                                    |> Async.Ignore
                                if isAllowedUser username then
                                    let parsedMessage = Control.parse botUsername text
                                    let requests = [
                                        for chat in settings.ChatsToMonitor do
                                            match parsedMessage with
                                            | Ban(UsernameList usernames, time) ->
                                                yield!
                                                    usernames
                                                    |> Seq.map ^ fun u ->
                                                        restrictUser context chat 1L time
                                            | Unban username ->
                                                ()
                                    ]
                                    let! text = Async.Parallel requests
                                    do! String.Join('\n', text)
                                        |> sprintf "Username: %s\n\n%s" username
                                        |> sendMessageToChannel
                            }
                        |> Option.defaultValue Async.Unit
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