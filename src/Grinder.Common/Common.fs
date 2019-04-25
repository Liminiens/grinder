namespace Grinder
       
[<RequireQualifiedAccess>]
module Task =
    open System.Threading.Tasks

    let inline Ignore (t: Task<_>) = t :> Task
    
[<AutoOpen>]
module Сommon =
    let inline (^) f x = f x

    let inline retype (x: 'a) : 'b = (# "" x : 'b #)

[<RequireQualifiedAccess>]
module Async =
    let Unit = async { () }