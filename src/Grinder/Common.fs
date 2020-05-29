namespace Grinder
    
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