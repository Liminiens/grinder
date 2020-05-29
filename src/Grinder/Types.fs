namespace Grinder.Types

open Funogram.Telegram.Types
open Grinder

type ChatsToMonitor = 
  ChatsToMonitor of string array      
    member __.Set =
      let (ChatsToMonitor chats) = __
      chats |> Set.ofArray

type AllowedUsers =
  AllowedUsers of string array
    member __.Set =
      let (AllowedUsers users) = __
      users |> Set.ofArray

type BotDefaultSettings = {
  Token: string
  ChatsToMonitor: ChatsToMonitor
  AllowedUsers: AllowedUsers
  ChannelId: int64
  AdminUserId: int64
}
    
type ReplyMessage = { Message: Message; ReplyToMessage: Message }

type UpdateType =
  | IgnoreMessage
  | NewReplyMessage of ReplyMessage
  | NewUsersAdded of User list
  | NewMessage of Message
  | NewAdminPrivateMessage of Document

[<RequireQualifiedAccess>]   
module UpdateType =
  let fromUpdate (settings: BotDefaultSettings) (update: Update) =
    update.Message
    |> Option.map ^ fun message ->
      if message.Chat.Id = settings.AdminUserId then
        match message.Document with
        | Some document ->
          NewAdminPrivateMessage document
        | None ->
          IgnoreMessage

      else
        let hasCodeBlock =
          message.Entities
          |> Option.filter (fun entities ->
            //code block
            entities
            |> Seq.exists ^ fun e -> e.Type = "code" && e.Offset = 0L
          )
          |> Option.isSome

        match message.NewChatMembers with
        | Some users ->
          NewUsersAdded(List.ofSeq users)

        | None ->
          if not hasCodeBlock then
            match message.ReplyToMessage with
            | Some reply ->
              { Message = message; ReplyToMessage = reply }
              |> NewReplyMessage

            | None ->
              NewMessage message
          else
            IgnoreMessage