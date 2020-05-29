namespace Grinder

open Hopac
open Hopac.Infixes

[<RequireQualifiedAccess>]
module UserStream =
  let private src = Stream.Src.create<DataAccess.User>()
  
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
      Datastore.upsertUsers users
    )