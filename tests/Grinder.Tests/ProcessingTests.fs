module Grinder.ProcessingTests

open FSharp.UMX
open Funogram
open Funogram.Types
open Xunit

open Grinder.Commands.Processing

let private replyToUser = {
    defaultUser with
        Id = 100500L
}

let private replyContext = {
    BotUsername = %"@testbot"
    Message = defaultMessage
    MessageText = "text"
    ReplyToUser = replyToUser
    ReplyToMessage = defaultMessage
    FromUsername = %"@user"
    ChatUsername = %"@user"
}

[<Fact>]
let ``parseReplyMessage properly parses the ban command``() =
    let inputMessage = "@testbot ban"
    let context = { replyContext with MessageText = inputMessage }
    let result = parseReplyMessage context
    match result with
    | BanOnReplyCommand commandContext -> Assert.Equal(commandContext.UserId, %replyToUser.Id)
    | _ -> Assert.Fail()

[<Fact>]
let ``parseReplyMessage properly parses the unban command``() =
    let inputMessage = "@testbot unban"
    let context = { replyContext with MessageText = inputMessage }
    let result = parseReplyMessage context
    match result with
    | UnbanOnReplyCommand commandContext -> Assert.Equal(commandContext.UserId, %replyToUser.Id)
    | _ -> Assert.Fail()
