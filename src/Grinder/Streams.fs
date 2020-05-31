namespace Grinder

open FSharp.Core
open Hopac
open Hopac.Infixes

[<RequireQualifiedAccess>]
module Stream =
  let inline chunkStream stream =
    stream
    |> Stream.groupByFun (fun _ ack group -> ack, group) (fun _ -> 0)
    |> Stream.mapJob (fun (ack, group) ->
      timeOutMillis 1000
      >>=. ack
      >>=. Stream.toSeq group
    )

[<RequireQualifiedAccess>]
module UserStream =
  let private src = Stream.Src.create<DataAccess.User>()
  let private consumer = IVar<seq<DataAccess.User> -> Job<unit>>()

  let setConsumer = IVar.fill consumer >> run

  let push user = Stream.Src.value src user

  do
    Stream.Src.tap src
    |> Stream.chunkStream
    |> Stream.consumeJob (fun users ->
      if IVar.Now.isFull consumer then
        (IVar.Now.get consumer) (users :> seq<_>)
      else
        Job.unit()
    )

[<RequireQualifiedAccess>]
module MessageStream =
  let private src = Stream.Src.create<DataAccess.Message>()
  let private consumer = IVar<seq<DataAccess.Message> -> Job<unit>>()

  let setConsumer = IVar.fill consumer >> run

  let push user = Stream.Src.value src user

  do
    Stream.Src.tap src
    |> Stream.chunkStream
    |> Stream.consumeJob (fun messages ->
      if IVar.Now.isFull consumer then
        (IVar.Now.get consumer) (messages :> seq<_>)
      else
        Job.unit()
    )