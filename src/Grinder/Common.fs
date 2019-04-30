namespace Grinder

open Funogram.Types
open System.Net.Http

type BotSettings = {
    Token: string
    ProxyClient: HttpClient
    ChatsToMonitor: string array
    AllowedUsers: string array
    ChannelId: int64
    AdminUserId: int64
}
    
type ReplyToMessage = { Message: Message; ReplyToMessage: Message }

type UpdateType =
    | IgnoreMessage
    | ReplyToMessage of ReplyToMessage
    | NewUsersAdded of User list
    | NewMessage of Message
    | NewAdminPrivateMessage of Document
        
[<RequireQualifiedAccess>]
module JsonNet =
    open System.IO
    open Newtonsoft.Json
    
    let deserializeFromStream<'T> (stream: Stream) = 
        use reader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(reader)
        let jsonSerializer = new JsonSerializer()
        jsonSerializer.Deserialize<'T>(jsonReader)
