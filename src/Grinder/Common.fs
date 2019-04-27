namespace Grinder

open System.Net.Http

type BotSettings = {
    Token: string
    ProxyClient: HttpClient
    ChatsToMonitor: string array
    AllowedUsers: string array
    ChannelId: int64
    AdminUserId: int64
}

[<RequireQualifiedAccess>]
module JsonNet =
    open System.IO
    open Newtonsoft.Json
    
    let deserializeFromStream<'T> (stream: Stream) = 
        use reader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(reader)
        let jsonSerializer = new JsonSerializer()
        jsonSerializer.Deserialize<'T>(jsonReader)
