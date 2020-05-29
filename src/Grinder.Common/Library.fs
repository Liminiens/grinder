namespace Grinder
open System

#nowarn "42"

[<AutoOpen>]
module Сommon =
  let inline (^) f x = f x

  let inline retype (x: 'a) : 'b = (# "" x : 'b #)
  
  let inline toNullable (x: 'a): Nullable<'a> = Nullable(x)

[<RequireQualifiedAccess>]      
module Result =
  let inline partition results =
    let oks = ResizeArray()
    let errors = ResizeArray()
    results
    |> Seq.iter(function
      | Ok v -> oks.Add v
      | Error err -> errors.Add err
    )
    oks.ToArray(), errors.ToArray()