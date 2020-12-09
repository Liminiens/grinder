namespace Grinder

open Serilog

[<AutoOpen>]
module Log =
    let mutable logger = Unchecked.defaultof<ILogger>
    let logInfo (str: string) = logger.Information str
    let logExn (exn: exn) (msg: string)  = logger.Error(exn, msg)
    let logErr (msg: string) = logger.Error msg
    let logDbg (str: string) = logger.Debug str
    let logFatal (msg: string) (exn: exn) = logger.Fatal(exn, msg)