namespace Grinder

[<AutoOpen>]
module Сommon =
    let (^) f x = f x

[<RequireQualifiedAccess>]
module Async =
    let Unit = async { () }