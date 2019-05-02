namespace Grinder
open System

[<AutoOpen>]
module Сommon =

    let inline (^) f x = f x

    let inline retype (x: 'a) : 'b = (# "" x : 'b #)
    
    let inline toNullable (x: 'a): Nullable<'a> = Nullable(x)
    
[<RequireQualifiedAccess>]
module Task =
    open System.Threading.Tasks

    let inline Ignore (t: Task<_>): Task = upcast t
    
[<RequireQualifiedAccess>]
module Async =
    let Unit = async { do () }
    
[<RequireQualifiedAccess>]
module String =
    let inline join separator (strings: string seq) =
        String.Join(separator, strings)
  
[<RequireQualifiedAccess>]      
module Result =
    let inline partition results =
        let oks = ResizeArray()
        let errors = ResizeArray()
        results
        |> Seq.iter(function
            | Ok v -> oks.Add v
            | Error err -> errors.Add err)
        oks.ToArray(), errors.ToArray()