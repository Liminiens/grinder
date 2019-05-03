namespace Grinder.Types

open FSharp.UMX
open Funogram.Types
open System.Net.Http

[<Measure>] type private userUsername
[<Measure>] type private chatUsername
[<Measure>] type private userId
[<Measure>] type private chatId

type ChatUsername = string<chatUsername>
type UserUsername = string<userUsername>
type TelegramUserId = int64<userId>
type TelegramChatId = int64<chatId>

type ChatsToMonitor =
    private ChatsToMonitor of string array
        static member Create chats = ChatsToMonitor chats
        
        member __.Set =
            let (ChatsToMonitor chats) = __
            chats |> Set.ofArray

type AllowedUsers =
    private AllowedUsers of string array
        static member Create users = AllowedUsers users
        
        member __.Set =
            let (AllowedUsers users) = __
            users |> Set.ofArray

type BotSettings = {
    Token: string
    ProxyClient: HttpClient
    ChatsToMonitor: ChatsToMonitor
    AllowedUsers: AllowedUsers
    ChannelId: TelegramChatId
    AdminUserId: TelegramUserId
}
    
type ReplyToMessage = { Message: Message; ReplyToMessage: Message }

type UpdateType =
    | IgnoreMessage
    | ReplyToMessage of ReplyToMessage
    | NewUsersAdded of User list
    | NewMessage of Message
    | NewAdminPrivateMessage of Document