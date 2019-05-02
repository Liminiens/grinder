module Grinder.FunogramExt

open System
open Funogram.Api
open Funogram.Bot
open Funogram.RequestsTypes
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
            do! Async.Sleep(TimeSpan.FromSeconds(2. + delay).Milliseconds)
            return! retry (times - 1) call
        else
            return Error e
}

let callApiWithRetry context times = api context.Config >> retry times

let callApiWithDefaultRetry context = callApiWithRetry context 5

[<RequireQualifiedAccess>]
module ApiExt =
    
    type RestrictChatMemberReqExt = 
        { ChatId: ChatId
          UserId: int64
          UntilDate: int64
          CanSendMessages: bool option
          CanSendMediaMessages: bool option
          CanSendOtherMessages: bool option
          CanAddWebPagePreviews: bool option }
        interface IRequestBase<bool> with
            member __.MethodName = "restrictChatMember"
             
    let restrictChatMemberBaseExt chatId userId (untilDate: DateTime) canSendMessages canSendMediaMessages canSendOtherMessages canAddWebPagePreviews =
        let seconds = int64 (untilDate.ToUniversalTime() - new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds;
        { ChatId = chatId; UserId = userId; UntilDate = seconds; CanSendMessages = canSendMessages; 
        CanSendMediaMessages = canSendMediaMessages; CanSendOtherMessages = canSendOtherMessages; CanAddWebPagePreviews = canAddWebPagePreviews }
        
    let restrictUser context chat username until = async {
        match! Datastore.findUserIdByUsername username with
        | UserIdFound userId ->
            let chat = sprintf "@%s" chat
            let! restrictResult =  
                restrictChatMemberBaseExt (Funogram.Types.String(chat)) userId until (Some false) (Some false) (Some false) (Some false)
                |> callApiWithDefaultRetry context
            match restrictResult with
            | Ok _ ->
                return Ok ()
            | Error e ->
                return Error <| sprintf "Failed to ban @%s in chat %s. Description: %s" username chat e.Description
        | UserIdNotFound ->
            return Error <| sprintf "Couldn't resolve username @%s" username
    }

    let restrictUserById context chat userId until = async {
        let chat = sprintf "@%s" chat
        let! restrictResult =  
            restrictChatMemberBaseExt (Funogram.Types.String(chat)) userId until (Some false) (Some false) (Some false) (Some false)
            |> callApiWithDefaultRetry context
        match restrictResult with
        | Ok _ ->
            return Ok ()
        | Error e ->
            return Error <| sprintf "Failed to ban %i in chat %s. Description: %s" userId chat e.Description
    }
            
    let unrestrictUser context chat username = async {
        match! Datastore.findUserIdByUsername username with
        | UserIdFound userId ->
            let chat = sprintf "@%s" chat
            let! restrictResult = 
                restrictChatMemberBase (Funogram.Types.String(chat)) userId None (Some true) (Some true) (Some true) (Some true)
                |> callApiWithDefaultRetry context
            match restrictResult with
            | Ok _ ->
                return Ok ()
            | Error e ->
                return Error <| sprintf "Failed to unban @%s in chat %s. Description: %s" username chat e.Description
        | UserIdNotFound ->
            return Error <| sprintf "Couldn't resolve username @%s" username
    }

    let sendMessage chatId context text =
        sendMessageBase (ChatId.Int chatId) text None None None None None
        |> callApiWithDefaultRetry context
        |> Async.Ignore  