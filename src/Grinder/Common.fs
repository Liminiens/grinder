namespace Grinder

open System.IO
open System.Net.Http

type BotSettings = {
    Token: string
    ProxyClient: HttpClient
    ChatsToMonitor: string array
    AllowedUsers: string array
    Channel: int64
    AdminUser: int64
}

module JsonNet =
    open Newtonsoft.Json
    
    let deserializeFromStream<'T> (stream: Stream) = 
        use reader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(reader)
        let jsonSerializer = new JsonSerializer()
        jsonSerializer.Deserialize<'T>(jsonReader)
