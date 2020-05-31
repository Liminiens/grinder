module Grinder.ProcessingTests

open Funogram
open Funogram.Telegram.Types
open Xunit

open Grinder.Commands.Processing

let private replyToUser = {
  defaultUser with Id = 100500L
}

let private replyContext = {
  BotUsername = "@testbot"
  Message = defaultMessage
  MessageText = "text"
  ReplyToUser = replyToUser
  ReplyToMessage = defaultMessage
  FromUsername = "@user"
  ChatUsername = "@user"
}

[<Fact>]
let ``parseReplyMessage properly parses the ban command``() =
  let inputMessage = "@testbot ban"
  let context = { replyContext with MessageText = inputMessage }
  let result = parseReplyMessage context
  match result with
  | BanOnReplyCommand commandContext -> Assert.Equal(replyToUser.Id, commandContext.UserId)
  | _ -> Assert.Fail()