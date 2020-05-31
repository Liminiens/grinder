namespace Grinder

open FSharp.Core
open Hopac
open Hopac.Infixes

[<RequireQualifiedAccess>]
module UserStream =
  let private src = Stream.Src.create<DataAccess.User>()
  let private consumer = IVar<seq<DataAccess.User> -> Job<unit>>()

  let setConsumer = IVar.fill consumer >> run

  let push user =
    Stream.Src.value src user

  do
    Stream.Src.tap src
    |> Stream.groupByFun (fun _ ack group -> ack, group) (fun _ -> 0)
    |> Stream.mapJob (fun (ack, group) ->
      timeOutMillis 1000
      >>=. ack
      >>=. Stream.toSeq group
    )
    |> Stream.consumeJob (fun users ->
      if IVar.Now.isFull consumer then
        (IVar.Now.get consumer) (users :> seq<_>)
      else
        Job.unit()
    )