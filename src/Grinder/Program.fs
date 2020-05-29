namespace Grinder

open Hopac
open Microsoft.Extensions.Configuration
open Grinder
open Grinder.DataAccess
open Grinder.Commands
open Grinder.Types
open Funogram.Telegram.Bot
open Funogram.Types
open FunogramExt
    
module Program =
  open System.Net.Http
  open MihaZupan
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
  
  let createHttpClient config =
    match config with
    | Some config ->
      let messageHandler = new HttpClientHandler()
      messageHandler.Proxy <- HttpToSocks5Proxy(config.Hostname, config.Port, config.Username, config.Password)
      messageHandler.UseProxy <- true
      new HttpClient(messageHandler)

    | None ->
      new HttpClient()
  
  let config =
    ConfigurationBuilder()
      .AddJsonFile("appsettings.json", false, true)
      .AddJsonFile("/etc/grinder/appsettings.json", true, true)
      .AddEnvironmentVariables("Grinder_")
      .Build()
      .Get<Config>()
      .Bot
          
  let botConfiguration = {
    defaultConfig with
      Token = config.Token
      Client = 
        match (box config.Socks5Proxy) with
        | null ->
          createHttpClient None

        | proxy -> 
          createHttpClient (Some (proxy :?> Socks5Configuration))
      AllowedUpdates = ["message"] |> Seq.ofList |> Some
  }

  let settings = {
    Token = config.Token
    ChatsToMonitor = ChatsToMonitor config.ChatsToMonitor
    AllowedUsers = AllowedUsers config.AllowedUsers
    ChannelId = config.ChannelId
    AdminUserId = config.AdminUserId
  }

  let sendTextToChannel text =
    ApiExt.sendMessage settings.ChannelId botConfiguration text

  let updateBox = Mailbox<UpdateContext>()

  let processUpdate context = 
    job {
      let updateType = UpdateType.fromUpdate settings context.Update
      match updateType with
      | Some newMessage ->
        match newMessage with
        | NewAdminUsersFileMessage document ->
          do! processAdminCommand settings context.Config document.FileId

        | NewUsersAddedToChat users ->
          do! processNewUsersAddedToChat users

        | NewMessage message ->
          match prepareTextMessage context.Me.Username message with
          | Some newMessage ->
            match authorize settings newMessage.FromUsername newMessage.ChatUsername with
            | CommandAllowed ->
              match! parseAndExecuteTextMessage settings context.Config newMessage with
              | Some message ->
                message
                |> formatMessage
                |> sendTextToChannel
                |> queueIgnore
              | None ->
                do! processCommonTextMessage message
            | CommandNotAllowed ->
              do! processCommonTextMessage message
          | None ->
            do! processCommonTextMessage message

        | NewReplyMessage reply ->
          match prepareReplyToMessage context.Me.Username reply with
          | Some replyMessage ->
            match authorize settings replyMessage.FromUsername replyMessage.ChatUsername with
            | CommandAllowed ->
              match! parseAndExecuteReplyMessage settings context.Config replyMessage with
              | Some message ->
                message
                |> formatMessage
                |> sendTextToChannel
                |> queueIgnore
              | None ->
                do! processCommonTextMessage reply.Message
            | CommandNotAllowed ->
              do! processCommonTextMessage reply.Message
          | None ->
            do! processCommonTextMessage reply.Message

        | CodeTextMessage user ->
          do! UserStream.push user

        | IgnoreMessage -> ()
      | None -> ()
    } |> queue

  [<EntryPoint>]
  let main _ =
    GrinderContext.MigrateUp()
    
    printfn "Starting bot"
    job {
      let onUpdate context =
        Mailbox.send updateBox context
        |> queue

      do! startBot botConfiguration onUpdate None
        
      printfn "Bot started"

      do! 
        Mailbox.take updateBox
        |> Job.map (fun update -> processUpdate update)
        |> Job.forever
    } 
    |> queue
    
    printfn "Bot exited"
    0 // return an integer exit code