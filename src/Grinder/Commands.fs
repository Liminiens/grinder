namespace Grinder.Commands

//always holds
#nowarn "0025"

open Hopac
open System
open Grinder
open Grinder.Types
open Funogram.Telegram.Types

module Parser =
  open FParsec

  type ParserError = string

  let str s = pstring s
  
  let pbotUsername botUsername : Parser<string, unit> =
    spaces >>. (str botUsername) .>> spaces
        
  let pusername: Parser<string, unit> =
    let validate char =
      (not <| Char.IsWhiteSpace(char)) && char <> '@'
    pipe2 (str "@") (manySatisfy validate) (+)

  type Usernames = Usernames of string array
  
  type TimeFraction =
    | Minutes of uint32
    | Days of uint32
    | Months of uint32
  
  type BanDuration =
    | Forever
    | Timed of DateTime
  
    member this.Value =
      match this with
      | Forever ->
        DateTime.UtcNow.AddMonths(13)
  
      | Timed date ->
        date
            
    member this.FormattedStringValue =
      this.Value.ToString("yyyy-MM-dd HH:mm:ss")
              
  type TimeFractionSummator() =
    let mutable value = Unchecked.defaultof<DateTime>
    
    member __.IsSet() =
      value <> Unchecked.defaultof<DateTime>
    
    member __.GetValue() =
      value
    
    member __.Add(fraction) =
      if not <| __.IsSet() then
        value <- DateTime.UtcNow
  
      match fraction with
      | Minutes mins ->
        value <- value.AddMinutes(float mins)
      | Days days ->
        value <- value.AddDays(float days)
      | Months months ->
        value <- value.AddMonths(int32 months)

  type BotUsernameCommandAction =
    | UsernameBan of BanDuration
    | UsernameUnban
  
  type BotUsernameCommand = Usernames * BotUsernameCommandAction

  type BotUsernameCommandParsingResult =
    | BotUsernameCommandResult of BotUsernameCommand
    | InvalidBotUsernameCommand of ParserError

  type BotReplyCommand =
    | TextBan

  type BotReplyCommandParsingResult =
    | BotReplyCommandResult of BotReplyCommand
    | InvalidBotReplyCommand of ParserError

  type AdminPrivateCommand =
    | Help
    | Config
    | AddAdminUser of username: string
    | RemoveAdminUser of username: string
    | AddChat of username: string
    | RemoveChat of username: string

  type AdminPrivateCommandResult =
    | AdminPrivateCommandResult of AdminPrivateCommand
    | InvalidAdminPrivateCommand of ParserError

  [<RequireQualifiedAccess>]
  module UsernameCommands =
    let pminutes: Parser<uint32, unit> =
      (puint32 .>> spaces) .>> (regex "min(s|utes)?")
      |>> (fun v -> if v < 5u then 6u else v)
        
    let pdays: Parser<uint32, unit> =
      (puint32 .>> spaces) .>> (regex "day(s)?")
        
    let pmonths: Parser<uint32, unit> =
      (puint32 .>> spaces) .>> (regex "month(s)?")
    
    let many1Usernames: Parser<string list, unit> =
      many1 (pusername .>> spaces)
    
    let sumTimedFractions (fractions: TimeFraction seq) =
      let summator = TimeFractionSummator()
      for fraction in fractions do
        summator.Add(fraction)
      Timed <| summator.GetValue()
        
    let pdistinctTimeFractions: Parser<BanDuration, unit> =
      [|
        pminutes |>> Minutes .>> spaces;
        pmonths |>> Months .>> spaces;
        pdays |>> Days .>> spaces
      |]
      |> Seq.map attempt
      |> choice
      |> many
      |>> Seq.distinct
      |>> sumTimedFractions
    
    let pforeverBan: Parser<BanDuration, unit> =
      [|
        spaces >>. eof >>% Forever;
        spaces >>. str "forever" >>% Forever;
      |]
      |> Seq.map attempt
      |> choice
    
    let pban: Parser<BanDuration, unit> =
      str "ban" .>> spaces >>. (pforeverBan <|> pdistinctTimeFractions)
    
    let punban: Parser<BotUsernameCommandAction, unit> =
      str "unban" .>> spaces >>% UsernameUnban
        
    let pcommandAction: Parser<BotUsernameCommandAction, unit> =
      (pban |>> UsernameBan) <|> punban
    
    let parseCommand botUsername =
      pbotUsername botUsername >>.
      pipe2 many1Usernames pcommandAction (fun usernames command -> 
        BotUsernameCommand(Usernames(Array.ofList usernames), command)
      )
        
    let runCommandParser botUsername text: ParserResult<BotUsernameCommand, unit> =
      run (parseCommand botUsername) text
    
    let parse botUsername text =
      match runCommandParser botUsername text with
      | Success(result, _, _)   ->
        BotUsernameCommandResult result
    
      | Failure(errorMsg, _, _) ->
        InvalidBotUsernameCommand errorMsg

  [<RequireQualifiedAccess>]
  module ReplyCommands =
    let pBanText: Parser<BotReplyCommand, unit> =
      str "/ban" >>% TextBan

    let pBotUsernameBan (botUsername: string): Parser<BotReplyCommand, unit> =
      pbotUsername botUsername .>> spaces >>.
      str "ban" >>% TextBan

    let parseCommand botUsername =
      (pBotUsernameBan botUsername) <|> pBanText
        
    let runCommandParser botUsername text: ParserResult<BotReplyCommand, unit> =
      run (parseCommand botUsername) text
    
    let parse botUsername text =
      match runCommandParser botUsername text with
      | Success(result, _, _)   ->
        BotReplyCommandResult result
    
      | Failure(errorMsg, _, _) ->
        InvalidBotReplyCommand errorMsg

  [<RequireQualifiedAccess>]
  module AdminPrivateCommands =
    let pHelpCommand: Parser<AdminPrivateCommand, unit> =
      str "/help" >>% Help

    let pConfigCommand: Parser<AdminPrivateCommand, unit> =
      str "/config" >>% Config

    let pAddAdminCommand: Parser<AdminPrivateCommand, unit> =
      str "/add_admin" .>> spaces >>. pusername |>> AddAdminUser

    let pRemoveAdminCommand: Parser<AdminPrivateCommand, unit> =
      str "/remove_admin" .>> spaces >>. pusername |>> RemoveAdminUser

    let pAddChatCommand: Parser<AdminPrivateCommand, unit> =
      str "/add_chat" .>> spaces >>. pusername |>> AddChat

    let pRemoveChatCommand: Parser<AdminPrivateCommand, unit> =
      str "/remove_chat" .>> spaces >>. pusername |>> RemoveChat

    let parseCommand =
      choice [|
        pHelpCommand
        pConfigCommand
        pAddAdminCommand
        pRemoveAdminCommand
        pAddChatCommand
        pRemoveChatCommand
      |]
      
    let runCommandParser text: ParserResult<AdminPrivateCommand, unit> =
      run parseCommand text
    
    let parse text =
      match runCommandParser text with
      | Success(result, _, _)   ->
        AdminPrivateCommandResult result
    
      | Failure(errorMsg, _, _) ->
        InvalidAdminPrivateCommand errorMsg

module Processing =
  open Funogram.Types
  open Grinder.FunogramExt
  open Parser
  
  type TextMessageContext = {
    BotUsername: string
    Message: Message
    MessageText: string
    FromUsername: string
    ChatUsername: string
  }
  
  type ReplyToMessageContext = {
    BotUsername: string
    Message: Message
    MessageText: string
    ReplyToUser: User
    ReplyToMessage: Message
    FromUsername: string
    ChatUsername: string
  }
  
  type AuthorizationResult =
    | CommandAllowed
    | CommandNotAllowed
  
  let authorize (botSettings: BotSettings) user chat =
    let isAllowedUser username =
      botSettings.AllowedUsers
      |> Set.contains username

    let isAllowedChat chatUsername =
      botSettings.ChatsToMonitor
      |> Set.contains chatUsername
        
    if isAllowedUser user && isAllowedChat chat then    
      CommandAllowed
    else
      CommandNotAllowed
  
  let prepareTextMessage (botUsername: string option) (message: Message): TextMessageContext option =
    botUsername
    |> Option.bind ^ fun botUsername ->
      message.From
      |> Option.map ^ fun from ->
        (botUsername, message, from)
    |> Option.bind ^ fun (botUsername, message, from) ->
      from.Username
      |> Option.map ^ fun username ->
        (botUsername, message, username)
    |> Option.bind ^ fun (botUsername, message, username) ->
      message.Text
      |> Option.map ^ fun text ->
        (botUsername, message, username, text)
    |> Option.bind ^ fun (botUsername, message, username, text) ->
      message.Chat.Username
      |> Option.map ^ fun chatUsername -> {
        BotUsername = (sprintf "@%s" botUsername)
        Message = message
        MessageText = text
        FromUsername = username
        ChatUsername = (sprintf "@%s" chatUsername)
      }
  
  let prepareReplyToMessage (botUsername: string option) (reply: ReplyMessage): ReplyToMessageContext option =
    botUsername
    |> Option.bind ^ fun botUsername ->
      reply.Message.From
      |> Option.map ^ fun from ->
        (botUsername, reply.Message, from)
    |> Option.bind ^ fun (botUsername, message, from) ->
      from.Username
      |> Option.map ^ fun username ->
        (botUsername, message, username)
    |> Option.bind ^ fun (botUsername, message, username) ->
      message.Text
      |> Option.map ^ fun text ->
        (botUsername, message, username, text)
    |> Option.bind ^ fun (botUsername, message, username, text) ->
      message.Chat.Username
      |> Option.map ^ fun chatUsername ->
        (chatUsername, botUsername, message, username, text)
    |> Option.bind ^ fun (chatUsername, botUsername, message, username, text) ->
      let singleChatMember =
        reply.ReplyToMessage.NewChatMembers
        |> Option.bind (fun members ->
          match members with
          | m when Seq.length m = 1 ->
            Seq.head m |> Some
          | _ -> None
        )

      if reply.ReplyToMessage.From = reply.Message.From then
        //user joined
        singleChatMember
        |> Option.map ^ fun user ->
           { BotUsername = (sprintf "@%s" botUsername)
             Message = message
             MessageText = text
             ReplyToUser = user
             ReplyToMessage = reply.ReplyToMessage
             FromUsername = username
             ChatUsername = (sprintf "@%s" chatUsername) }
      else
        match reply.ReplyToMessage.From with
        | Some from -> 
          { BotUsername = (sprintf "@%s" botUsername)
            Message = message
            MessageText = text
            ReplyToUser = from
            ReplyToMessage = reply.ReplyToMessage
            FromUsername = username
            ChatUsername = (sprintf "@%s" chatUsername) }
          |> Some

        | None ->
          singleChatMember
          |> Option.map ^ fun user ->
            { BotUsername = (sprintf "@%s" botUsername)
              Message = message
              MessageText = text
              ReplyToUser = user
              ReplyToMessage = reply.ReplyToMessage
              FromUsername = username
              ChatUsername = (sprintf "@%s" chatUsername) }
                    
  type ActionOnReplyCommandContext = {
    From: string
    MessageId: int64
    ReplyToMessageId: int64
    ChatId: int64
    UserId: int64
    Username: string option
  }

  type BanCommandContext = {
    From: string
    MessageId: int64
    ChatId: int64
    Usernames: string array
    Until: BanDuration
  }
  
  type UnbanCommandContext = {
    From: string
    MessageId: int64
    ChatId: int64
    Usernames: string array
  }
  
  type Command =
    | BanCommand of BanCommandContext
    | BanOnReplyCommand of ActionOnReplyCommandContext
    | UnbanCommand of UnbanCommandContext
    | DoNothingCommand
  
  type CommandError =
    | ApiError of string
    | AdminBanNotAllowedError of string
    | CouldNotResolveUsername of string
  
  type IMessage =
    abstract member FormatAsString: unit -> string
  
  type BanOnReplyMessage =
    { Username: string
      UserId: int64
      Chats: Set<string> }

    interface IMessage with
      member this.FormatAsString() =
        let chatsText =
          this.Chats
          |> String.concat ", "
                
        sprintf "Banned %i (%s) in chats %s forever" this.UserId this.Username chatsText
  
  type BanMessage =
    { Usernames: string array
      Chats: Set<string>
      Until: BanDuration }

    interface IMessage with
      member this.FormatAsString() =
        let durationText =
          if this.Until.Value > DateTime.UtcNow.AddYears(1) then
            "forever"
          else
            sprintf "until %s UTC" this.Until.FormattedStringValue
                
        let usernamesText =
          this.Usernames
          |> String.concat ", "
            
        let chatsText =
          this.Chats
          |> String.concat ", "
                
        sprintf "Banned %s in chats %s %s" usernamesText chatsText durationText
              
  type UnbanMessage =
    { Usernames: string seq
      Chats: string seq }
    
    interface IMessage with
      member this.FormatAsString() =
        let usernamesText =
          this.Usernames
          |> String.concat ", "
            
        let chatsText =
          this.Chats
          |> String.concat ", "

        sprintf "Unbanned %s in chats %s" usernamesText chatsText
  
  type CommandMessage = 
    | BanMessage of string * BanMessage * CommandError array
    | BanOnReplyMessage of string * BanOnReplyMessage * CommandError array
    | UnbanMessage of string * UnbanMessage * CommandError array
      
  let parseReplyMessage (context: ReplyToMessageContext): Command =
    match ReplyCommands.parse context.BotUsername context.MessageText with
    | BotReplyCommandResult TextBan ->
      let context = {
        ActionOnReplyCommandContext.From = context.FromUsername
        MessageId = context.Message.MessageId
        ReplyToMessageId = context.ReplyToMessage.MessageId
        ChatId = context.Message.Chat.Id
        UserId = context.ReplyToUser.Id
        Username = 
          context.ReplyToUser.Username 
          |> Option.map(fun u -> sprintf "@%s" u)
      }
      BanOnReplyCommand context

    | InvalidBotReplyCommand _ -> 
      DoNothingCommand
                          
  let parseTextMessage (context: TextMessageContext): Command =
    match Parser.UsernameCommands.parse context.BotUsername context.MessageText with
    | BotUsernameCommandResult((Usernames usernames), UsernameBan(duration)) ->
      let context = {
        From = context.FromUsername
        MessageId = context.Message.MessageId
        ChatId = context.Message.Chat.Id
        Usernames = usernames
        Until = duration
      }
      BanCommand context

    | BotUsernameCommandResult((Usernames usernames), UsernameUnban) ->
      let context = {
        From = context.FromUsername
        MessageId = context.Message.MessageId
        ChatId = context.Message.Chat.Id
        Usernames = usernames
      }
      UnbanCommand context

    | InvalidBotUsernameCommand _ ->
      DoNothingCommand
       
  let authorizeChat (settings: BotSettings) chatUsername =
    chatUsername
    |> Option.map settings.ChatsToMonitor.Contains
    |> Option.defaultValue false

  let inline usernameToStringOrNull (username: string option) =
    match username with
    | Some u -> u
    | None -> null

  let executeCommand config (botSettings: BotSettings) command: Job<CommandMessage option> = job {
    let getErrors results =
      results
      |> Result.partition
      |> snd
    
    let createCommandError fn text: Result<unit, CommandError> =
      text |> (fn >> Result.Error)
        
    let userCanBeBanned username =
      botSettings.AllowedUsers
      |> Set.contains username
      |> not

    let deleteThreeMessagesInChats userId =
      job {
        let! messagesInChats = Datastore.getLastThreeMessagesInChats userId
        return!
          messagesInChats
          |> Seq.collect(fun chat ->
            chat.Messages
            |> Seq.map(fun message ->
              ApiExt.deleteMessageWithRetry config chat.ChatId message.MessageId
            )
          )
          |> Job.conIgnore
      }

    match command with
    | BanCommand context ->
      ApiExt.deleteMessageWithRetry config context.ChatId context.MessageId 
      |> queueIgnore
      
      let! requests = 
        job {
          let! (userIds, usernames) =
            context.Usernames
            |> Seq.filter userCanBeBanned
            |> Seq.map (fun username ->
              job {
                match! Datastore.findUserIdByUsername username with
                | UserIdFound id -> 
                  return Choice1Of2 id
                | UserIdNotFound -> 
                  return Choice2Of2 username
              }
            )
            |> Job.conCollect
            |> Job.map (fun result ->
              result
              |> Array.ofSeq 
              |> Array.partition (function Choice1Of2 _ -> true | Choice2Of2 _ -> false)
              |> (fun (ids, usernames) ->
                ids |> Array.map (fun (Choice1Of2 id) -> id),
                usernames |> Array.map (fun (Choice2Of2 username) -> username)
              )
            )

          //delete messages
          userIds
          |> Seq.map deleteThreeMessagesInChats
          |> Job.conIgnore
          |> queue

          return [|
            for userId in userIds do
              for chat in botSettings.ChatsToMonitor do
                yield 
                  ApiExt.banUserByUserId config chat userId context.Until.Value
                  |> Job.map ^ Result.mapError ApiError

            for username in usernames do
              let text = sprintf "Couldn't resolve username %s" username
              yield Error(CouldNotResolveUsername text) |> Job.result
          |]
        }
       
      let! errors =
        requests
        |> Job.conCollect
        |> Job.map getErrors
              
      let message = {
        Chats = botSettings.ChatsToMonitor
        Usernames = context.Usernames
        Until = context.Until
      }
      
      return Some <| BanMessage(context.From, message, errors)

    | BanOnReplyCommand context ->
      ApiExt.deleteMessageWithRetry config context.ChatId context.MessageId 
      |> queueIgnore
      
      do
        let username = usernameToStringOrNull context.Username
        
        DataAccess.User(UserId = context.UserId, Username = username)
        |> UserStream.push
        |> queue

      let! username = job {
        match context.Username with
        | Some username ->
          return username

        | None ->
          let! username = Datastore.getUsernameByUserId context.UserId
          return 
            username
            |> Option.map (sprintf "@%s")
            |> Option.defaultValue "unknown user"
      }

      let requests =
        if userCanBeBanned username then
          ApiExt.deleteMessageWithRetry config context.ChatId context.ReplyToMessageId 
          |> queueIgnore

          deleteThreeMessagesInChats context.UserId
          |> queue

          [|
            for chat in botSettings.ChatsToMonitor do
              yield 
                ApiExt.banUserByUserId config chat context.UserId (DateTime.UtcNow.AddMonths(13))
                |> Job.map ^ Result.mapError ApiError
          |]

        elif username <> "unkown user" then
          sprintf "Cannot ban admin %s" username
          |> createCommandError AdminBanNotAllowedError
          |> Job.result
          |> Array.singleton

        else
          Array.empty
              
      let! errors =
        requests
        |> Job.conCollect
        |> Job.map getErrors
      
      let message = {
        Chats = botSettings.ChatsToMonitor
        Username = username
        UserId = context.UserId
      }
      
      return Some <| BanOnReplyMessage(context.From, message, errors)

    | UnbanCommand context ->
      ApiExt.deleteMessageWithRetry config context.ChatId context.MessageId 
      |> queueIgnore
      
      let requests = [|
        for username in context.Usernames do
          for chat in botSettings.ChatsToMonitor do
            yield 
              ApiExt.unbanUserByUsername config chat username
              |> Job.map ^ Result.mapError ApiError
          
            yield 
              ApiExt.unrestrictUserByUsername config chat username
              |> Job.map ^ Result.mapError ApiError
      |]

      let! errors =
        requests
        |> Job.conCollect
        |> Job.map getErrors
      
      let message = {
        Chats = botSettings.ChatsToMonitor
        Usernames = context.Usernames
      }
      
      return Some <| UnbanMessage(context.From, message, errors)

    | DoNothingCommand ->
      return None
  }
  
  let parseAndExecuteTextMessage settings config message: Job<CommandMessage option> =
    parseTextMessage message
    |> executeCommand config settings
  
  let parseAndExecuteReplyMessage settings config message: Job<CommandMessage option> =
    parseReplyMessage message
    |> executeCommand config settings
      
  let processCommonTextMessage (message: Message) =
    job {
      match message.From with
      | Some user ->
        let username = usernameToStringOrNull user.Username

        let userToUpdate = DataAccess.User(UserId = user.Id, Username = username)
        let message = DataAccess.Message(MessageId = message.MessageId, ChatId = message.Chat.Id, UserId = user.Id)
        do! UserStream.push userToUpdate
        do! MessageStream.push message
      | None -> ()
    }

  let processAdminCommand (botSettings: BotSettings) (config: BotConfig) fileId: Job<unit> = 
    job {
      match! ApiExt.prepareAndDownloadFile config fileId with
      | Ok stream ->
        try
          let users = JsonNet.deserializeFromStream<DataAccess.User[]>(stream)
          
          do!
            users
            |> Seq.chunkBySize 200
            |> Seq.map Datastore.upsertUsers
            |> Job.seqIgnore

          return!
            ApiExt.sendMessage botSettings.ChannelId config "Updated user database"
            |> Job.Ignore

        with e ->
          return!
            ApiExt.sendMessage botSettings.ChannelId config (e.ToString())
            |> Job.Ignore

      | Error e ->
        return!
          ApiExt.sendMessage botSettings.ChannelId config e
          |> Job.Ignore
    }
  
  let processNewUsersAddedToChat (users: User seq): Job<unit> =
    users
    |> Seq.map (fun u ->
      let username = usernameToStringOrNull u.Username
      DataAccess.User(UserId = u.Id, Username = username)
      |> UserStream.push
    )
    |> Job.conIgnore
  
  let formatMessage: CommandMessage -> string =
    let concatErrors (errors: CommandError seq) =
      let errorsText = [|
        for error in errors do
          match error with
          | ApiError e -> e
          | AdminBanNotAllowedError e -> e
          | CouldNotResolveUsername e -> e
      |]
      match errorsText with
      | [||] ->
        String.Empty

      | text ->
        text
        |> String.concat "\n"
        |> sprintf "\n\n%s"
    
    let formatHeader commandName username (message: IMessage) =
      sprintf "%s command from: @%s\n\n%s" commandName username (message.FormatAsString())
        
    function
    | BanMessage(fromUsername, message, errors) ->
      sprintf "%s%s" (formatHeader "Ban" fromUsername message) (concatErrors errors)

    | UnbanMessage(fromUsername, message, errors) ->
      sprintf "%s%s" (formatHeader "Unban" fromUsername message) (concatErrors errors)

    | BanOnReplyMessage(fromUsername, message, errors) ->
      sprintf "%s%s" (formatHeader "Ban on reply" fromUsername message) (concatErrors errors)