namespace Grinder

open Grinder.DataAccess
open Grinder
open Grinder.Commands
open Funogram.Api
open Funogram.Bot
open Funogram.Types
    
module Program =
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
        ChannelId: int64
        AdminUserId: int64
    }
    
    let createHttpClient config =
        let messageHandler = new HttpClientHandler()
        messageHandler.Proxy <- HttpToSocks5Proxy(config.Hostname, config.Port, config.Username, config.Password)
        messageHandler.UseProxy <- true
        new HttpClient(messageHandler)
    
    type NewMessageType =
        | IgnoreMessage
        | NewUsersAdded of User list
        | NewMessage of Message
        | NewAdminPrivateMessage of Document
    
    module NewMessageType =
        let fromUpdate (settings: BotSettings)  (update: Update)=
            update.Message
            |> Option.map ^ fun message ->
                if message.Chat.Id = settings.AdminUserId then
                    match message.Document with
                    | Some document ->
                        NewAdminPrivateMessage document
                    | None ->
                        IgnoreMessage
                else
                    match message.NewChatMembers with
                    | Some users ->
                        NewUsersAdded(List.ofSeq users)
                    | None ->
                        NewMessage message
                
    let onUpdate (settings: BotSettings) (context: UpdateContext) =
        async {
            do! NewMessageType.fromUpdate settings context.Update
                |> Option.map ^ fun newMessage -> async {
                    match newMessage with
                    | NewAdminPrivateMessage document ->
                        do! Processing.iterAdminCommand settings context document.FileId
                    | NewUsersAdded users ->
                        do! Processing.iterNewUsersCommand users
                    | NewMessage message ->
                        do! Processing.iterTextMessage (Processing.processTextCommand settings) context message
                    | IgnoreMessage ->
                        ()
                }
                |> Option.defaultValue Async.Unit
        } |> Async.Start
        
    [<EntryPoint>]
    let main _ =
        let config =
            File.ReadAllText(Path.Combine(Directory.GetCurrentDirectory(), "bot_config.json"))
            |> JsonConvert.DeserializeObject<BotConfig>
        
        let client = createHttpClient config.Socks5Proxy
        
        let botConfiguration = { 
            defaultConfig with
                Token = config.Token
                Client = client
                AllowedUpdates = ["message"] |> Seq.ofList |> Some
        }

        GrinderContext.MigrateUp()
        
        async {
            printfn "Starting bot"
            
            let settings = {
                Token = config.Token
                ProxyClient = client
                ChatsToMonitor = config.ChatsToMonitor
                AllowedUsers = config.AllowedUsers
                ChannelId = config.ChannelId
                AdminUserId = config.AdminUserId
            }
            do! startBot botConfiguration (onUpdate settings) None
                |> Async.StartChild
                |> Async.Ignore
                
            printfn "Bot started"
            do! Async.Sleep(-1)
        } |> Async.RunSynchronously
        
        printfn "Bot exited"
        0 // return an integer exit code