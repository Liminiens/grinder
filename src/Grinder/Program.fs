namespace Grinder

open System.Net
open Microsoft.Extensions.Configuration
open Funogram
open Grinder
open Grinder.DataAccess
open Grinder.Commands
open Grinder.Types
open Funogram.Api
open Funogram.Bot
open Funogram.Types
open FunogramExt
open System
    
module Program =
    open System.Net.Http
    open MihaZupan
    open FSharp.UMX
    open Processing
    
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
    
    [<CLIMutable>]
    type Config = {
        Bot: BotConfig
    }
    
    let createHttpClient (config: Socks5Configuration) =
        let messageHandler = new HttpClientHandler()
        if not (isNull (box config)) then
            messageHandler.Proxy <- HttpToSocks5Proxy(config.Hostname, config.Port, config.Username, config.Password)
            messageHandler.UseProxy <- true
        new HttpClient(messageHandler)
   
    let createBotApi config (settings: BotSettings) = {
        new IBotApi with
            member __.DeleteMessage chatId messageId =
                Api.deleteMessage %chatId %messageId
                |> callApiWithDefaultRetry config
                |> Async.Ignore
            
            member __.BanUserByUsername chatUsername userUsername until =
                ApiExt.banUserByUsername config %chatUsername %userUsername until
                
            member __.BanUserByUserId chatUsername userId until =
                ApiExt.banUserByUserId config %chatUsername %userId until
                
            member __.UnbanUser chatUsername username =
                ApiExt.unbanUserByUsername config %chatUsername %username
                
            member __.UnrestrictUser chatUsername username =
                ApiExt.unrestrictUserByUsername config %chatUsername %username
                
            member __.SendTextToChannel text =
                ApiExt.sendMessage settings.ChannelId config text
            
            member __.PrepareAndDownloadFile fileId =
                ApiExt.prepareAndDownloadFile config fileId
    }
    
    let dataApi = {
        new IDataAccessApi with
            member __.GetUsernameByUserId userId = async {
                match! Datastore.findUsernameByUserId %userId with
                | UsernameFound username ->
                    return Some %(sprintf "@%s" username)
                | UsernameNotFound ->
                    return None
            }
            
            member __.UpsertUsers users =
                Datastore.upsertUsers users
    }    
                
    let onUpdate (settings: BotSettings) (botApi: IBotApi) (dataApi: IDataAccessApi) (context: UpdateContext) =
        async {
            do! UpdateType.fromUpdate settings context.Update
                |> Option.map ^ fun newMessage -> async {
                    match newMessage with
                    | NewAdminPrivateMessage document ->
                        do! processAdminCommand settings context.Config document.FileId
                    | NewUsersAdded users ->
                        do! processNewUsersCommand users
                    | NewMessage message ->
                        match prepareTextMessage context.Me.Username message with
                        | Some newMessage ->
                            match authorize settings newMessage.FromUsername newMessage.ChatUsername with
                            | CommandAllowed ->
                                let! command = parseAndExecuteTextMessage settings botApi dataApi newMessage
                                do! command
                                    |> Option.map (formatMessage >> botApi.SendTextToChannel)
                                    |> Option.defaultValue Async.Unit
                            | CommandNotAllowed -> ()
                        | None -> ()
                    | NewReplyMessage reply ->
                        match prepareReplyToMessage context.Me.Username reply with
                        | Some replyMessage ->
                            match authorize settings replyMessage.FromUsername replyMessage.ChatUsername with
                            | CommandAllowed ->
                                let! command = parseAndExecuteReplyMessage settings botApi dataApi replyMessage
                                do! command
                                    |> Option.map (formatMessage >> botApi.SendTextToChannel)
                                    |> Option.defaultValue Async.Unit
                            | CommandNotAllowed -> ()
                        | None -> ()
                    | IgnoreMessage -> ()
                }
                |> Option.defaultValue Async.Unit
        } |> Async.Start
        
    [<EntryPoint>]
    let main _ =
        let mutable configBuilder =
            ConfigurationBuilder()
                .AddJsonFile("appsettings.json", false, true)
                .AddJsonFile("/etc/grinder/appsettings.json", true, true)
                .AddEnvironmentVariables("Grinder_")
                
        match Environment.GetEnvironmentVariable("DOTNETRU_APP_CONFIG") with
        | null -> ()
        | connString ->
            configBuilder <- configBuilder
                .AddAzureAppConfiguration connString
                
        let config =
            configBuilder
                .Build()
                .Get<Config>()
                .Bot;
                
        let botConfiguration = {
            defaultConfig with
                Token = config.Token
                Client = createHttpClient config.Socks5Proxy
                AllowedUpdates = ["message"] |> Seq.ofList |> Some
        }

        GrinderContext.MigrateUp()
        
        printfn "Starting bot"
        
        let settings = {
            Token = config.Token
            ChatsToMonitor = ChatsToMonitor.Create config.ChatsToMonitor
            AllowedUsers = AllowedUsers.Create config.AllowedUsers
            ChannelId = %config.ChannelId
            AdminUserId = %config.AdminUserId
        }
        startBot botConfiguration (onUpdate settings (createBotApi botConfiguration settings) dataApi) None
        |> Async.Start
            
        printfn "Bot started"
        
        use listener = new HttpListener()
        listener.Prefixes.Add("http://*:80/")
        listener.Start()
        
        let buffer = System.Text.Encoding.UTF8.GetBytes "OK"
        
        while true do
            let ctx = listener.GetContext()
            let output = ctx.Response.OutputStream
            output.Write(buffer, 0, buffer.Length)
            output.Close();
        
        printfn "Bot exited"
        0 // return an integer exit code