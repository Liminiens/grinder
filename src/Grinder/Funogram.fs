module Grinder.FunogramExt

open Hopac
open System
open Funogram
open Funogram.Api
open Funogram.Telegram.Types
open Funogram.Telegram.Api
open Funogram.Telegram
open Funogram.Types

let private jitter = new Random()

let rec private retry times (call: Job<Result<'T, 'TError>>) = 
  job {
    match! call with 
    | Ok ok -> 
      return Ok ok
    | Error e ->
      if times <> 0 then
        let delay = jitter.Next(1, 4) |> float
        do! timeOut (TimeSpan.FromSeconds(2. + delay))
        return! retry (times - 1) call
      else
        return Error e
  }

let callApiWithRetry config times request = 
  Job.fromAsync(api config request) |> retry times

let callApiWithDefaultRetry config = callApiWithRetry config 5

[<RequireQualifiedAccess>]
module ApiExt =
  type UnbanChatMemberReq = 
    { ChatId: ChatId
      UserId: int64 }
    interface IRequestBase<bool> with
        member __.MethodName = "unbanChatMember"
          
  type KickChatMemberReqExt = 
    { ChatId: ChatId
      UserId: int64
      UntilDate: int64 }
    interface IRequestBase<bool> with
        member __.MethodName = "kickChatMember"
      
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
  
  let restrictPermissions = 
    { ChatPermissions.CanAddWebPagePreviews = Some false
      CanSendMessages = Some false
      CanSendMediaMessages = Some false 
      CanSendPools = Some false 
      CanSendOtherMessages = Some false
      CanChangeInfo = Some false 
      CanInviteUsers = Some false 
      CanPinMessages = Some false }

  let unbanChatMemberByChatNameExt chatName userId : UnbanChatMemberReq =
    { ChatId = ChatId.String chatName; UserId = userId }
           
  let restrictChatMemberBaseExt chatId userId (untilDate: DateTime) canSendMessages canSendMediaMessages canSendOtherMessages canAddWebPagePreviews =
    let seconds = int64 (untilDate.ToUniversalTime() - new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds
    { ChatId = chatId; UserId = userId; UntilDate = seconds; CanSendMessages = canSendMessages; 
    CanSendMediaMessages = canSendMediaMessages; CanSendOtherMessages = canSendOtherMessages; CanAddWebPagePreviews = canAddWebPagePreviews }
  
  let kickChatMemberByChatNameUntilExt chatName userId (untilDate: DateTime): KickChatMemberReqExt =
    let seconds = int64 (untilDate.ToUniversalTime() - new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds
    { ChatId = (ChatId.String chatName); UserId = userId; UntilDate = seconds }
      
  let banUserByUsername context chat username until = 
    job {
      match! Datastore.findUserIdByUsername username with
      | UserIdFound userId ->
        let! banResult =  
            kickChatMemberByChatNameUntilExt chat userId until
            |> callApiWithDefaultRetry context
        match banResult with
        | Ok _ ->
            return Ok ()
        | Error e ->
            return Error <| sprintf "Failed to ban %s in chat %s. Description: %s" username chat e.Description
      | UserIdNotFound ->
        return Error <| sprintf "Couldn't resolve username %s" username
    }

  let banUserByUserId context chat userId until = 
    job {
      let! restrictResult =  
        kickChatMemberByChatNameUntilExt chat userId until
        |> callApiWithDefaultRetry context
      match restrictResult with
      | Ok _ ->
        return Ok ()
      | Error e ->
        return Error <| sprintf "Failed to ban %i in chat %s. Description: %s" userId chat e.Description
    }
          
  let unbanUserByUsername context chat username = 
    job {
      match! Datastore.findUserIdByUsername username with
      | UserIdFound userId ->
        let! unbanResult = 
          unbanChatMemberByChatNameExt chat userId
          |> callApiWithDefaultRetry context
        match unbanResult with
        | Ok _ ->
          return Ok ()
        | Error e ->
          return Error <| sprintf "Failed to unban %s in chat %s. Description: %s" username chat e.Description
      | UserIdNotFound ->
        return Error <| sprintf "Couldn't resolve username %s" username
    }
  
  let unrestrictUserByUsername context chat username = 
    job {
      match! Datastore.findUserIdByUsername username with
      | UserIdFound userId ->
        let! restrictResult = 
            restrictChatMemberBase (ChatId.String chat) userId restrictPermissions None
            |> callApiWithDefaultRetry context
        match restrictResult with
        | Ok _ ->
            return Ok ()
        | Error e ->
            return Error <| sprintf "Failed to unrestrict %s in chat %s. Description: %s" username chat e.Description

      | UserIdNotFound ->
        return Error <| sprintf "Couldn't resolve username %s" username
    }
  
  let sendMessage (chatId: int64) context text =
    sendMessageBase (ChatId.Int chatId) text None None None None None
    |> callApiWithDefaultRetry context
      
  let prepareAndDownloadFile config fileId =
    job {
      match! getFile fileId |> callApiWithDefaultRetry config with
      | Ok data ->
        let uri =
          let filePath = Option.get data.FilePath
          sprintf "https://api.telegram.org/file/bot%s/%s" config.Token filePath
        try
          let! stream = config.Client.GetStreamAsync(uri) |> Async.AwaitTask
          return Ok stream

        with
        | e ->
          return Error <| e.ToString()

      | Error e ->
        return Error <| sprintf "Failed to download file. Description: %s" e.Description
    }
    |> retry 5

  let deleteMessageWithRetry config chatId messageId =
    Api.deleteMessage chatId messageId
    |> callApiWithDefaultRetry config