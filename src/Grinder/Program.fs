namespace Grinder

open Hopac
open System.Net
open Microsoft.Extensions.Configuration
open Grinder
open Grinder.DataAccess
open Grinder.Commands
open Grinder.Types
open Funogram.Api
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Types
open FunogramExt
open System
    
module Program =
  open System.Net.Http
  open MihaZupan
  open Processing
  
  let createHttpClient (config: Socks5Configuration) =
    let messageHandler = new HttpClientHandler()
    if not (isNull (box config)) then
      messageHandler.Proxy <- HttpToSocks5Proxy(config.Hostname, config.Port, config.Username, config.Password)
      messageHandler.UseProxy <- true
    new HttpClient(messageHandler)
  
  let botConfig =
    let mutable configBuilder =
      ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false, true)
        .AddJsonFile("/etc/grinder/appsettings.json", true, true)
        .AddEnvironmentVariables("Grinder_")
          
    match Environment.GetEnvironmentVariable("DOTNETRU_APP_CONFIG") with
    | null -> ()
    | connString ->
       configBuilder <- configBuilder.AddAzureAppConfiguration connString
         
    configBuilder
      .Build()
      .Get<Config>()
      .Bot
          
  let funogramConfig = {
    defaultConfig with
      Token = botConfig.Token
      Client = createHttpClient botConfig.Socks5Proxy
      AllowedUpdates = ["message"] |> Seq.ofList |> Some
  }

  let sendTextToChannel channelId text =
    ApiExt.sendMessage funogramConfig channelId text

  let processUpdate context = 
    job {
      let! settings = Configuration.getCurrentSettings()
      let updateType = UpdateType.fromUpdate settings context.Update

      match updateType with
      | Some newMessage ->
        match newMessage with
        | NewAdminPrivateMessage(chatId, text) ->
          do! parseAndExecutePrivateCommand context.Config settings chatId text

        | NewAdminUsersFileMessage(chatId, document) ->
          match document.FileName, document.MimeType with
          | Some name, Some mimeType when name.StartsWith("users_export") && mimeType = "application/json" ->
            do! processAdminCommand settings context.Config document.FileId

          | _ ->
            Api.sendMessage chatId "Expecting file starting with users_export with mime type application/json"
            |> api context.Config
            |> Job.fromAsync
            |> queueIgnore

        | NewUsersAddedToChat(users, chatUsername) ->
          if authorizeChat settings chatUsername then
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
                |> sendTextToChannel settings.ChannelId
                |> queueIgnore

              | None ->
                do! processCommonTextMessage message

            | CommandNotAllowed ->
              if authorizeChat settings (Some newMessage.ChatUsername) then
                do! processCommonTextMessage message
              
              ApiExt.deleteMessage context.Config newMessage.Message.Chat.Id newMessage.Message.MessageId
              |> queueIgnore

          | None ->
            if authorizeChat settings message.Chat.Username then
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
                |> sendTextToChannel settings.ChannelId
                |> queueIgnore

              | None ->
                do! processCommonTextMessage reply.Message

            | CommandNotAllowed ->
              if authorizeChat settings reply.Message.Chat.Username then
                do! processCommonTextMessage reply.Message

              ApiExt.deleteMessage context.Config reply.Message.Chat.Id reply.Message.MessageId
              |> queueIgnore

          | None ->
            if authorizeChat settings reply.Message.Chat.Username then
              do! processCommonTextMessage reply.Message

        | UsualMessage(messageId, chatId, userId, username, chatUsername) ->
          if authorizeChat settings chatUsername then
            let user = DataAccess.User(UserId = userId, Username = username)
            let message = DataAccess.Message(MessageId = messageId, ChatId = chatId, UserId = userId)
            do! UserStream.push user
            do! MessageStream.push message

        | IgnoreMessage -> ()
      | None -> ()
    } |> queue

  let startAzureHealthcheckJob() =
    let buffer = System.Text.Encoding.UTF8.GetBytes "OK"
    let listener = new HttpListener()
    listener.Prefixes.Add("http://*:80/")
    listener.Start()
    job {
      let! ctx = listener.GetContextAsync()
      let output = ctx.Response.OutputStream
      output.Write(buffer, 0, buffer.Length)
      output.Close()
    }
    |> Job.forever
    |> queue

  [<EntryPoint>]
  let main _ =
    GrinderContext.MigrateUp()
    
    Configuration.setBotConfig botConfig
    UserStream.setConsumer Datastore.upsertUsers
    MessageStream.setConsumer Datastore.insertMessages
    Datastore.startMessageCleanupJob()

    printfn "Starting bot"

    startBot funogramConfig processUpdate None |> Job.fromAsync |> queue
      
    printfn "Bot started"

    startAzureHealthcheckJob()
    
    Console.ReadKey() |> ignore
    printfn "Bot exited"
    0 // return an integer exit code