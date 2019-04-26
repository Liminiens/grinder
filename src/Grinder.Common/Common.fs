namespace Grinder

[<AutoOpen>]
module Сommon =
    open System

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