namespace Grinder.Commands

open Hopac
open System
open Grinder
open Grinder.Types
open Funogram.Telegram.Types

module Parser =
  open FParsec

  type Usernames = Usernames of string array
  
  type ParserError = string
  
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
              
  type CommandAction =
    | Ban of BanDuration
    | Unban

  type Command = Command of Usernames * CommandAction

  let str s = pstring s
      
  let pminutes: Parser<uint32, unit> =
    (puint32 .>> spaces) .>> (regex "min(s|utes)?")
    |>> (fun v -> if v < 5u then 6u else v)
      
  let pdays: Parser<uint32, unit> =
    (puint32 .>> spaces) .>> (regex "day(s)?")
      
  let pmonths: Parser<uint32, unit> =
    (puint32 .>> spaces) .>> (regex "month(s)?")
      
  let pusername: Parser<string, unit> =
    let validate char =
      (not <| Char.IsWhiteSpace(char)) && char <> '@'
    pipe2 (str "@") (manySatisfy validate) (+)

  let pbotUsername botUsername : Parser<string, unit> =
    spaces >>. (str botUsername) .>> spaces
    
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

  let punban: Parser<CommandAction, unit> =
    str "unban" .>> spaces >>% Unban
      
  let pcommandAction: Parser<CommandAction, unit> =
    (pban |>> Ban) <|> punban

  let parseCommand botUsername =
    pbotUsername botUsername >>.
    pipe2 many1Usernames pcommandAction (fun usernames command -> Command(Usernames(Array.ofList usernames), command))
      
  let runCommandParser botUsername str: ParserResult<Command, unit> =
    run (parseCommand botUsername) str
  
  type CommandParsingResult =
    | BotCommand of Command
    | InvalidCommand of ParserError
  
  let parse botUsername text =
    match runCommandParser botUsername text with
    | Success(result, _, _)   ->
      BotCommand result

    | Failure(errorMsg, _, _) ->
      InvalidCommand errorMsg
            
module Processing =
  open Funogram
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
  
  let authorize (botSettings: BotDefaultSettings) user chat =
    let isAllowedUser username =
      botSettings.AllowedUsers.Set
      |> Set.contains username

    let isAllowedChat chatUsername =
      botSettings.ChatsToMonitor.Set
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
      reply.ReplyToMessage.From
      |> Option.map (fun user ->
        { BotUsername = (sprintf "@%s" botUsername)
          Message = message
          MessageText = text
          ReplyToUser = user
          ReplyToMessage = reply.ReplyToMessage
          FromUsername = username
          ChatUsername = (sprintf "@%s" chatUsername) }
      )
                    
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
    let messageText = context.MessageText
    let botMentioned = messageText.StartsWith(context.BotUsername)
    
    // bot nickname and command are delimited by space
    let hasUnbanWord = messageText.Contains(" unban")
    let hasBanWord = messageText.Contains(" ban") 
    
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
            
    match (botMentioned, hasBanWord, hasUnbanWord) with
    | (true, true, false) -> BanOnReplyCommand context
    | _ -> DoNothingCommand
                          
  let parseTextMessage (context: TextMessageContext): Command =
    match Parser.parse context.BotUsername context.MessageText with
    | BotCommand(Command((Usernames usernames), Ban(duration))) ->
      let context = {
        From = context.FromUsername
        MessageId = context.Message.MessageId
        ChatId = context.Message.Chat.Id
        Usernames = usernames
        Until = duration
      }
      BanCommand context

    | BotCommand(Command((Usernames usernames), Unban)) ->
      let context = {
        From = context.FromUsername
        MessageId = context.Message.MessageId
        ChatId = context.Message.Chat.Id
        Usernames = usernames
      }
      UnbanCommand context

    | InvalidCommand _ ->
      DoNothingCommand
              
  let executeCommand config (botSettings: BotDefaultSettings) command: Job<CommandMessage option> = job {
    let getErrors results =
      results
      |> Result.partition
      |> snd
    
    let createCommandError fn text: Result<unit, CommandError> =
      text |> (fn >> Result.Error)
        
    let userCanBeBanned username =
      botSettings.AllowedUsers.Set
      |> Set.contains username
      |> not
    
    match command with
    | BanCommand context ->
      ApiExt.deleteMessageWithRetry config context.ChatId context.MessageId 
      |> queueIgnore
      
      let requests = [|
        for username in context.Usernames do
          if userCanBeBanned username then
            for chat in botSettings.ChatsToMonitor.Set do
              yield 
                ApiExt.banUserByUsername config chat username context.Until.Value
                |> Job.map (fun result ->
                  Result.mapError ApiError result
                )
          else
            yield 
              sprintf "Cannot ban admin @%s" username
              |> createCommandError AdminBanNotAllowedError
              |> Job.result
      |]
       
      let! errors =
        requests
        |> Job.conCollect
        |> Job.map getErrors
              
      let message = {
        Chats = botSettings.ChatsToMonitor.Set
        Usernames = context.Usernames
        Until = context.Until
      }
      
      return Some <| BanMessage(context.From, message, errors)

    | BanOnReplyCommand context ->
      ApiExt.deleteMessageWithRetry config context.ChatId context.MessageId 
      |> Job.Ignore
      |> queue

      ApiExt.deleteMessageWithRetry config context.ChatId context.ReplyToMessageId 
      |> Job.Ignore
      |> queue
      
      do
        let username = 
          match context.Username with
          | Some username -> username
          | None -> null
        
        DataAccess.User(UserId = context.UserId, Username = username)
        |> UserStream.pushUser
        |> queue

      let! username = job {
        match context.Username with
        | Some username ->
          return username

        | None ->
          let! username = Datastore.getUsernameByUserId context.UserId
          return 
            username
            |> Option.map ^ fun name -> (sprintf "@%s" name)
            |> Option.defaultValue "unknown user"
      }

      let requests =
        if userCanBeBanned username then
          [|
            for chat in botSettings.ChatsToMonitor.Set do
              yield 
                ApiExt.banUserByUserId config chat context.UserId (DateTime.UtcNow.AddMonths(13))
                |> Job.map ^ fun result ->
                  Result.mapError ApiError result
          |]
        else
          [|
            sprintf "Cannot ban admin %s" username
            |> createCommandError AdminBanNotAllowedError
            |> Job.result
          |]
              
      let! errors =
        requests
        |> Job.conCollect
        |> Job.map getErrors
      
      let message = {
        Chats = botSettings.ChatsToMonitor.Set
        Username = username
        UserId = context.UserId
      }
      
      return Some <| BanOnReplyMessage(context.From, message, errors)

    | UnbanCommand context ->
      ApiExt.deleteMessageWithRetry config context.ChatId context.MessageId 
      |> Job.Ignore
      |> queue
      
      let requests = [|
        for username in context.Usernames do
          for chat in botSettings.ChatsToMonitor.Set do
            yield 
              ApiExt.unbanUserByUsername config chat username
              |> Job.map (fun result ->
                Result.mapError ApiError result
              )
          
            yield 
              ApiExt.unrestrictUserByUsername config chat username
              |> Job.map (fun result ->
                Result.mapError ApiError result
              )
      |]

      let! errors =
        requests
        |> Job.conCollect
        |> Job.map getErrors
      
      let message = {
        Chats = botSettings.ChatsToMonitor.Set
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
      
  let processAdminCommand (botSettings: BotDefaultSettings) (config: BotConfig) fileId: Job<unit> = 
    job {
      match! ApiExt.prepareAndDownloadFile config fileId with
      | Ok stream ->
        try
          let users = JsonNet.deserializeFromStream<DataAccess.User[]>(stream)
          do! Datastore.upsertUsers users
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
    |> Seq.filter ^ fun u -> Option.isSome u.Username
    |> Seq.map (fun u ->
      DataAccess.User(UserId = u.Id, Username = Option.get u.Username)
      |> UserStream.pushUser
    )
    |> Job.conIgnore
  
  let formatMessage: CommandMessage -> string =
    let concatErrors (errors: CommandError seq) =
      let errorsText = [|
        for error in errors do
          match error with
          | ApiError e -> yield e
          | AdminBanNotAllowedError e -> yield e
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