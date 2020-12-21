namespace Grinder

open System.IO
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
open Serilog
    
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
            member __.DeleteMessage chatId messageId: Async<unit> =
                sprintf "Deleting message %A in chat %A" messageId chatId
                |> logInfo
                Api.deleteMessage %chatId %messageId
                |> callApiWithDefaultRetry config
                |> Async.Map (function
                    | Ok _ -> () // ignoring
                    | Error apiError ->
                        sprintf "Error %s with code %d on deleting message %A in chat %A"
                            apiError.Description
                            apiError.ErrorCode
                            messageId
                            chatId
                        |> logErr
                )
            
            member __.BanUserByUsername chatUsername userUsername until: Async<Result<unit, string>> =
                sprintf "Banning user %A by name in chat %A until %A" userUsername chatUsername until
                |> logInfo
                ApiExt.banUserByUsername config %chatUsername %userUsername until
                
            member __.BanUserByUserId chatUsername userId until: Async<Result<unit, string>> =
                sprintf "Banning userId %A in chat %A until %A" userId chatUsername until
                |> logInfo
                ApiExt.banUserByUserId config %chatUsername %userId until
                
            member __.UnbanUser chatUsername username: Async<Result<unit, string>> =
                sprintf "Unbanning user %A in chat %A" username chatUsername
                |> logInfo
                ApiExt.unbanUserByUsername config %chatUsername %username
                
            member __.SendTextMessage chatId text: Async<unit> =
                sprintf "Sending {%s} into chat %A" text chatId
                |> logInfo
                ApiExt.sendMessage chatId config text
                |> Async.Catch
                |> Async.Map (function
                    | Choice1Of2 () -> ()
                    | Choice2Of2 e ->
                        sprintf "Error on sending {%s} into chat %A" text chatId
                        |> logExn e
                )

            member __.UnrestrictUser chatUsername username: Async<Result<unit, string>> =
                sprintf "Unrestricting user %A in chat %A"username chatUsername
                |> logInfo
                ApiExt.unrestrictUserByUsername config %chatUsername %username
                
            member __.SendTextToChannel text: Async<unit> =
                sprintf "Sending {%s} into special channel %A" text settings.ChannelId
                |> logInfo
                ApiExt.sendMessage settings.ChannelId config text
                |> Async.Catch
                |> Async.Map (function
                    | Choice1Of2 () -> ()
                    | Choice2Of2 e ->
                        sprintf "Error on sending {%s} into special channel %A" text settings.ChannelId
                        |> logExn e
                )
            
            member __.PrepareAndDownloadFile fileId: Async<Result<IO.Stream, string>> =
                sprintf "Downloading file %A" fileId
                |> logInfo
                ApiExt.prepareAndDownloadFile config fileId
    }
    
    let dataApi = {
        new IDataAccessApi with
            member __.GetUsernameByUserId userId = async {
                match! Datastore.findUsernameByUserId %userId with
                | UsernameFound username ->
                    return Some %(sprintf "@%s" username)
                | UsernameNotFound ->
                    sprintf "User %A hasn't been found in Datastore" userId
                    |> logErr
                    return None
            }
            
            member __.UpsertUsers users =
                Datastore.upsertUsers users
                |> Async.Catch
                |> Async.Map (function
                    | Choice1Of2 () -> ()
                    | Choice2Of2 e ->
                        sprintf "Error on upserting new users %A" users
                        |> logExn e
                )
    }    
                
    let onUpdate (settings: BotSettings) (botApi: IBotApi) (dataApi: IDataAccessApi) (context: UpdateContext) =
        async {
            do! UpdateType.fromUpdate settings context.Update
                |> Option.map ^ fun newMessage -> async {
                    match newMessage with
                    | NewAdminPrivateMessage document ->
                        sprintf "Received: New admin private message with fileId: %s" document.FileId
                        |> logInfo
                        do! processAdminCommand settings context.Config document.FileId
                        
                    | NewUsersAdded users ->
                        sprintf "Received: New users added %A" users
                        |> logInfo
                        do! processNewUsersCommand users
                        
                    | NewMessage message ->
                        sprintf "Received: New message in chat %s from %s"
                            (defaultArg message.Chat.Title "")
                            (defaultArg (message.From |> Option.bind(fun x -> x.Username)) "")
                        |> logDbg
                        
                        match prepareTextMessage context.Me.Username message with
                        | Some newMessage ->
                            match authorize settings newMessage.FromUsername newMessage.ChatUsername with
                            | CommandAllowed ->
                                let! command = parseAndExecuteTextMessage settings botApi dataApi newMessage
                                do! command
                                    |> Option.map (formatMessage >> botApi.SendTextToChannel)
                                    |> Option.defaultValue Async.Unit
                            | CommandNotAllowed ->
                                sprintf "Command %s NOT allowed for user %A in chat %A"
                                    newMessage.MessageText
                                    newMessage.FromUsername
                                    newMessage.ChatUsername
                                |> logDbg
                        | None ->
                            sprintf "Skipping message %A from %A" message context.Me.Username
                            |> logDbg
                        
                    | NewReplyMessage reply ->
                        sprintf "Received: New reply message in chat %s from %A"
                            (defaultArg reply.Message.Chat.Title "")
                            (defaultArg (reply.Message.From |> Option.bind(fun x -> x.Username)) "")
                        |> logDbg
                        
                        match prepareReplyToMessage context.Me.Username reply with
                        | Some replyMessage ->
                            match authorize settings replyMessage.FromUsername replyMessage.ChatUsername with
                            | CommandAllowed ->
                                let! command = parseAndExecuteReplyMessage settings botApi dataApi replyMessage
                                do! command
                                    |> Option.map (formatMessage >> botApi.SendTextToChannel)
                                    |> Option.defaultValue Async.Unit
                            | CommandNotAllowed ->
                                sprintf "Command %s NOT allowed for user %A in chat %A"
                                    replyMessage.MessageText
                                    replyMessage.FromUsername
                                    replyMessage.ChatUsername
                                |> logDbg
                        | None ->
                            sprintf "Skipping message %A from %A" reply context.Me.Username
                            |> logDbg
                    | IgnoreMessage ->
                        sprintf "Ignoring message %A" context.Update.Message
                        |> logDbg
                }
                |> Option.defaultValue Async.Unit
        } |> Async.Start
        
    [<EntryPoint>]
    let main _ =
        logger <- LoggerConfiguration()
            .MinimumLevel.Information()
            .WriteTo.Console()
            .CreateLogger();
        
        let mutable configBuilder =
            ConfigurationBuilder()
                .AddJsonFile("appsettings.json", false, true)
                .AddJsonFile("/etc/grinder/appsettings.json", true, true)
                .AddEnvironmentVariables("Grinder_")
        
        match Environment.GetEnvironmentVariable("VAHTER_CONFIG") with
        | null -> ()
        | config ->
            configBuilder <- configBuilder
                .AddJsonStream(new MemoryStream(System.Text.Encoding.ASCII.GetBytes config))
        
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
        
        let settings = {
            Token = config.Token
            ChatsToMonitor = ChatsToMonitor.Create config.ChatsToMonitor
            AllowedUsers = AllowedUsers.Create config.AllowedUsers
            ChannelId = %config.ChannelId
            AdminUserId = %config.AdminUserId
        }

        string { botConfiguration with Token = "***" }
        |> sprintf "Bot Configuration %A"
        |> logInfo
        
        string { settings with Token = "***" }
        |> sprintf "Bot Settings %A"
        |> logInfo
        
        logInfo "Starting bot"
        startBot botConfiguration (onUpdate settings (createBotApi botConfiguration settings) dataApi) None
        |> Async.Start

        logInfo "Bot started"
        
        // Needed for azure web app deploy check. We have to response with anything on port 80
        use listener = new HttpListener()
        listener.Prefixes.Add("http://*:80/")
        listener.Start()
        
        let buffer = System.Text.Encoding.UTF8.GetBytes "OK"
        
        while true do
            let ctx = listener.GetContext()
            let output = ctx.Response.OutputStream
            output.Write(buffer, 0, buffer.Length)
            output.Close()
            logInfo "Sending OK on HTTP request"
        
        logInfo "Bot exited"
        0 // return an integer exit code