namespace Grinder.Types

open FSharp.UMX
open System
open System.IO
open Funogram.Types
open Grinder

[<Measure>] type private userUsername
[<Measure>] type private chatUsername
[<Measure>] type private userId
[<Measure>] type private chatId
[<Measure>] type private messageId

type ChatUsername = string<chatUsername>
type UserUsername = string<userUsername>
type TelegramUserId = int64<userId>
type TelegramChatId = int64<chatId>
type TelegramMessageId = int64<messageId>

type ChatsToMonitor =
    private ChatsToMonitor of ChatUsername array
        static member Create (chats: string array) =
            chats
            |> Array.map (fun chat -> %chat)
            |> ChatsToMonitor
        
        member __.Set =
            let (ChatsToMonitor chats) = __
            chats |> Set.ofArray

type AllowedUsers =
    private AllowedUsers of UserUsername array
        static member Create (users: string array) =
            users
            |> Array.map (fun chat -> %chat)
            |> AllowedUsers
        
        member __.Set =
            let (AllowedUsers users) = __
            users |> Set.ofArray

type BotSettings = {
    Token: string
    ChatsToMonitor: ChatsToMonitor
    AllowedUsers: AllowedUsers
    ChannelId: TelegramChatId
    AdminUserId: TelegramUserId
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
    let fromUpdate (settings: BotSettings) (update: Update) =
        update.Message
        |> Option.bind ^ fun message ->
            message.Entities
            |> Option.filter ^ fun entities ->
                //skip code examples
                entities
                |> Seq.exists ^ fun e -> e.Type = "code" && e.Offset = 0L
                |> not
            |> Option.map ^ fun _ ->
                if message.Chat.Id = %settings.AdminUserId then
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
                        match message.ReplyToMessage with
                        | Some reply ->
                            { Message = message; ReplyToMessage = reply }
                            |> NewReplyMessage
                        | None ->
                            NewMessage message
    
type IBotApi =
    abstract member DeleteMessage: TelegramChatId -> TelegramMessageId -> Async<unit>
    abstract member BanUserByUsername: ChatUsername -> UserUsername -> DateTime -> Async<Result<unit, string>>
    abstract member BanUserByUserId: ChatUsername -> TelegramUserId -> DateTime -> Async<Result<unit, string>>
    abstract member UnbanUser: ChatUsername -> UserUsername -> Async<Result<unit, string>>
    abstract member UnrestrictUser: ChatUsername -> UserUsername -> Async<Result<unit, string>>
    abstract member SendTextToChannel: string -> Async<unit>
    abstract member PrepareAndDownloadFile: string -> Async<Result<Stream, string>>