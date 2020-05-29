namespace Grinder

open Hopac
open Funogram.Types

[<RequireQualifiedAccess>]
module Config =
  let private config = IVar<BotConfig>()

  let set conf = IVar.fill config conf |> run

  let get = IVar.read config