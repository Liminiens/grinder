namespace Grinder
     
open Hopac
open Funogram.Types

[<RequireQualifiedAccess>]
module JsonNet =
  open System.IO
  open Newtonsoft.Json
  
  let deserializeFromStream<'T> (stream: Stream) = 
    use reader = new StreamReader(stream)
    use jsonReader = new JsonTextReader(reader)
    let jsonSerializer = new JsonSerializer()
    jsonSerializer.Deserialize<'T>(jsonReader)

[<RequireQualifiedAccess>]
module Config =
  let private config = IVar<BotConfig>()

  let set conf = IVar.fill config conf |> start

  let get = IVar.read config