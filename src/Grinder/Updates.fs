namespace Grinder.Types

open Funogram.Telegram.Types
open Grinder
    
type ReplyMessage = { Message: Message; ReplyToMessage: Message }

type UpdateType =
  | IgnoreMessage
  | UsualMessage of messageId: int64 * chatId: int64 * userId: int64 * username: string * chatUsername: string option
  | NewReplyMessage of ReplyMessage
  | NewUsersAddedToChat of User list * chatUsername: string option
  | NewMessage of Message
  | NewAdminUsersFileMessage of chatId: int64 * Document

[<RequireQualifiedAccess>]   
module UpdateType =
  let fromUpdate (settings: BotSettings) (update: Update) =
    update.Message
    |> Option.map (fun message ->
      if message.Chat.Id = settings.AdminUserId then
        match message.Document, message.Text with
        | Some document, None ->
          NewAdminUsersFileMessage(message.Chat.Id, document)

        | _ ->
          IgnoreMessage

      else
        let hasCodeBlock =
          message.Entities
          |> Option.filter (fun entities ->
            //code block
            entities
            |> Seq.exists (fun e -> e.Type = "code" && e.Offset = 0L)
          )
          |> Option.isSome

        match message.NewChatMembers with
        | Some users ->
          NewUsersAddedToChat(List.ofSeq users, message.Chat.Username)

        | None ->
          if not hasCodeBlock then
            match message.ReplyToMessage with
            | Some reply ->
              { Message = message; ReplyToMessage = reply }
              |> NewReplyMessage

            | None ->
              NewMessage message
          else
            match message.From with
            | Some user ->
              let username =
                match user.Username with
                | Some username -> username
                | None -> null
              UsualMessage(message.MessageId, message.Chat.Id, user.Id, username, message.Chat.Username)

            | None ->
              IgnoreMessage
    )