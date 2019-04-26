module Grinder.FunogramExt

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

let callApiWithDefaultRetry context = callApiWithRetry context 10

[<RequireQualifiedAccess>]
module ApiExt = 
    let restrictUser context chat username until = async {
        match Datastore.findUserIdByUsername username with
        | UserIdFound userId ->
            do! restrictChatMemberBase (Funogram.Types.String(chat)) userId (Some until) (Some false) (Some false) (Some false) (Some false)
                |> callApiWithDefaultRetry context
                |> Async.Ignore
            let dateText = until.ToString("yyyy-MM-dd")
            return sprintf "Banned in chat %s until %s UTC" chat dateText
        | UserIdNotFound ->
            return "Couldn't resolve username"
    }
            
    let unrestrictUser context chat username = async {
        match Datastore.findUserIdByUsername username with
        | UserIdFound userId ->
            let! userResponse = 
                getChatMemberByChatName chat userId
                |> callApiWithDefaultRetry context
            match userResponse with
            | Ok _ ->
                let time = DateTime.UtcNow.AddMinutes(float 1) |> Some
                do! restrictChatMemberBase (Funogram.Types.String(chat)) userId time (Some true) (Some true) (Some true) (Some true)
                    |> callApiWithDefaultRetry context
                    |> Async.Ignore      
                return sprintf "Unbanned in chat %s" chat
            | Error _ ->
                return sprintf "Not unbanned\found in chat %s" chat
        | UserIdNotFound ->
            return "Couldn't resolve username"
    }

    let sendMessage chatId context text = async {
        do! sendMessageBase (ChatId.Int chatId) text None None None None None
            |> callApiWithDefaultRetry context
            |> Async.Ignore  
    }