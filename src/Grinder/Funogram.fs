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
            let delay = jitter.Next(1, 4) |> float
            do! Task.Delay(TimeSpan.FromSeconds(2. + delay)) |> Async.AwaitTask
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
            let chat = sprintf "@%s" chat
            let! restrictResult =  
                restrictChatMemberBase (Funogram.Types.String(chat)) userId (Some until) (Some false) (Some false) (Some false) (Some false)
                |> callApiWithDefaultRetry context
            match restrictResult with
            | Ok _ ->
                let dateText = until.ToString("yyyy-MM-dd")
                return sprintf "Banned in chat %s until %s UTC" chat dateText
            | Error e ->
                return sprintf "Failed to ban in chat %s. Description: %s" chat e.Description
        | UserIdNotFound ->
            return sprintf "Couldn't resolve username %s" username
    }
            
    let unrestrictUser context chat username = async {
        match Datastore.findUserIdByUsername username with
        | UserIdFound userId ->
            let chat = sprintf "@%s" chat
            let! restrictResult = 
                restrictChatMemberBase (Funogram.Types.String(chat)) userId None (Some true) (Some true) (Some true) (Some true)
                |> callApiWithDefaultRetry context
            match restrictResult with
            | Ok _ ->
                return sprintf "Unbanned in chat %s" chat
            | Error e ->
                return sprintf "Failed to unban in chat %s. Description: %s" chat e.Description
        | UserIdNotFound ->
            return sprintf "Couldn't resolve username %s" username
    }

    let sendMessage chatId context text = async {
        do! sendMessageBase (ChatId.Int chatId) text None None None None None
            |> callApiWithDefaultRetry context
            |> Async.Ignore  
    }