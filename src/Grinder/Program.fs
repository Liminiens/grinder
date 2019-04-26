namespace Grinder

open Grinder.DataAccess
open Grinder
open Grinder.Commands
open Funogram.Api
open Funogram.Bot
open Funogram.Types
    
module Program =
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
    
    type NewMessageType =
        | NewUsersAdded of User list * Message
        | NewMessage of Message
    
    module NewMessageType =
        let fromUpdate (update: Update) =
            update.Message
            |> Option.map ^ fun message ->
                match message.NewChatMembers with
                | Some users ->
                    NewUsersAdded(List.ofSeq users, message)
                | None ->
                    NewMessage message
                
    let onUpdate (settings: BotSettings) (context: UpdateContext) =
        async {
            do! NewMessageType.fromUpdate context.Update
                |> Option.map ^ fun newMessage -> async {
                    match newMessage with
                    | NewUsersAdded(users, message) ->
                        ()
                    | NewMessage message ->
                        do! Processing.iterTextMessage (Processing.processTextCommand settings) context message
                }
                |> Option.defaultValue Async.Unit
        } |> Async.Start
        
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

        GrinderContext.MigrateUp()
        
        async {
            printfn "Starting bot"
            
            let settings = {
                ChatsToMonitor = config.ChatsToMonitor
                AllowedUsers = config.AllowedUsers
                Channel = config.Channel
            }
            do! startBot botConfiguration (onUpdate settings) None
                |> Async.StartChild
                |> Async.Ignore
                
            printfn "Bot started"
            do! Task.Delay(Timeout.InfiniteTimeSpan) |> Async.AwaitTask
        } |> Async.RunSynchronously
        
        printfn "Bot exited"
        0 // return an integer exit code