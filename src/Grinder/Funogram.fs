module Grinder.Funogram

open System
open System.Threading.Tasks
open Funogram.Api
open Funogram.Bot
open Funogram.Types

let private jitter = new Random()

let rec private retry times (call: Async<Result<'T, ApiResponseError>>) = async {
    match! call with 
    | Ok ok -> 
        return Ok ok
    | Error e ->
        printfn "Api call error: %A; ErrorCode: %i" e.Description e.ErrorCode
        if times <> 0 then
            let delay = jitter.Next(0, 3) |> float
            do! Task.Delay(TimeSpan.FromSeconds(1. + delay)) |> Async.AwaitTask
            return! retry (times - 1) call
        else
            return Error e
}

let callApiWithRetry context times = api context.Config >> retry times