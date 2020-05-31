module Grinder.CommandsTests

open FSharp.Core
open System
open System.Collections
open System.Collections.Generic
open Funogram.Telegram.Types
open Grinder.Commands.Processing
open Grinder.Types
open Foq
open Grinder.Commands.Parser
open Xunit

type NotValidTextMessageGenerator() =
    interface IEnumerable<obj array> with
        member __.GetEnumerator() =
            let s =
                seq {
                    yield { defaultMessage
                                with Text = Some "text"
                                     Chat = defaultChat
                                     From =  Some { defaultUser with Username = Some "user" } }
                    yield { defaultMessage
                                with Text = None
                                     Chat = { defaultChat with Username = Some "chat" }
                                     From =  Some { defaultUser with Username = Some "user" } }
                    yield { defaultMessage
                                with Text = None
                                     Chat = { defaultChat with Username = Some "chat" }
                                     From =  Some defaultUser }
                    yield { defaultMessage
                                with Text = None
                                     Chat = { defaultChat with Username = Some "chat" }
                                     From =  None }
                }
                |> Seq.map (fun i -> [|box i|])
            s.GetEnumerator()
            
    interface IEnumerable with
        member __.GetEnumerator() =
            (__ :> IEnumerable<_>).GetEnumerator() :> IEnumerator

type NotValidReplyMessageGenerator() =
    interface IEnumerable<obj array> with
        member __.GetEnumerator() =
            let s =
                seq {
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  None };
                          ReplyToMessage =
                              { defaultMessage
                                with NewChatMembers = None
                                     From =  None } }
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  Some { defaultUser with Username = Some "user" } };
                          ReplyToMessage =
                              { defaultMessage
                                with NewChatMembers = None
                                     From =  None } }
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  None };
                          ReplyToMessage =
                              { defaultMessage
                                with NewChatMembers = None
                                     From =  Some { defaultUser with Username = Some "user" } } }
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  None };
                          ReplyToMessage =
                              { defaultMessage
                                 with NewChatMembers = Some (Seq.singleton { defaultUser with Username = Some "user" }) } }
                }
                |> Seq.map (fun i -> [|box i|])
            s.GetEnumerator()
            
    interface IEnumerable with
        member __.GetEnumerator() =
            (__ :> IEnumerable<_>).GetEnumerator() :> IEnumerator
            
[<Fact>]
let ``prepareTextMessage returns Some``() =
    let botUsername = Some "bot"
    let message = 
        { defaultMessage
            with Text = Some "text"
                 Chat = { defaultChat with Username = Some "chat" }
                 From =  Some { defaultUser with Username = Some "user" } }
    match prepareTextMessage botUsername message with
    | Some textMessage ->
        Assert.Equal(textMessage.BotUsername, "@bot")
        Assert.Equal(textMessage.ChatUsername, "@chat")
        Assert.Equal(textMessage.FromUsername, "user")
        Assert.Equal(textMessage.MessageText, "text")
        Assert.Equal(textMessage.Message, message)
    | None ->
        Assert.Fail()

[<Theory>]
[<ClassData(typeof<NotValidTextMessageGenerator>)>]
let ``prepareTextMessage returns None``(message: Message) =
    let botUsername = Some "bot"
    match prepareTextMessage botUsername message with
    | Some _ ->
        Assert.Fail()
    | None ->
        Assert.Success()
        
[<Fact>]
let ``prepareTextMessage returns None when bot username is None``() =
    match prepareTextMessage None defaultMessage with
    | Some _ ->
        Assert.Fail()
    | None ->
        Assert.Success()
        
[<Fact>]
let ``prepareReplyMessage returns Some when reply is to message``() =
    let botUsername = Some "bot"
    let message = 
        { defaultMessage
            with Text = Some "text"
                 Chat = { defaultChat with Username = Some "chat" }
                 From =  Some { defaultUser with Username = Some "user" } }
        
    let replyToUser = { defaultUser with Username = Some "user1" }
    
    let reply = 
        { defaultMessage
            with From =  Some replyToUser }
        
    match prepareReplyToMessage botUsername { Message = message; ReplyToMessage = reply } with
    | Some replyMessage ->
        Assert.Equal(replyMessage.BotUsername, "@bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(replyToUser, replyMessage.ReplyToUser)
        Assert.Equal(replyMessage.Message, message)
    | None ->
        Assert.Fail()
        
[<Fact>]
let ``prepareReplyMessage returns Some when someone added chat member``() =
    let botUsername = Some "bot"
    let user = { defaultUser with Username = Some "user" } 
    let newUser = { defaultUser with Username = Some "user1" }
    let message = 
        { defaultMessage
            with Text = Some "text"
                 Chat = { defaultChat with Username = Some "chat" }
                 From =  Some user }
    
    let reply = 
        { defaultMessage
            with From =  Some user
                 NewChatMembers = Some (Seq.singleton newUser) }
        
    match prepareReplyToMessage botUsername { Message = message; ReplyToMessage = reply } with
    | Some replyMessage ->
        Assert.Equal(replyMessage.BotUsername, "@bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(newUser, replyMessage.ReplyToUser)
        Assert.Equal(replyMessage.Message, message)
    | None ->
        Assert.Fail()

[<Fact>]
let ``prepareReplyMessage returns Some when new chat member``() =
    let botUsername = Some "bot"
    let newUser = { defaultUser with Username = Some "user1" }
    let message = 
        { defaultMessage
            with Text = Some "text"
                 Chat = { defaultChat with Username = Some "chat" }
                 From =  Some { defaultUser with Username = Some "user" }  }
    
    let reply = 
        { defaultMessage
            with NewChatMembers = Some (Seq.singleton newUser) }
        
    match prepareReplyToMessage botUsername { Message = message; ReplyToMessage = reply } with
    | Some replyMessage ->
        Assert.Equal(replyMessage.BotUsername, "@bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(replyMessage.ReplyToUser, newUser)
        Assert.Equal(replyMessage.Message, message)
    | None ->
        Assert.Fail()
        
[<Fact>]
let ``prepareReplyMessage returns Some when message is from someone``() =
    let botUsername = Some "bot"
    let newUser = { defaultUser with Username = Some "user1" }
    let message = 
        { defaultMessage
            with Text = Some "text"
                 Chat = { defaultChat with Username = Some "chat" }
                 From =  Some { defaultUser with Username = Some "user" }  }
    
    let reply = 
        { defaultMessage
            with From = Some newUser }
        
    match prepareReplyToMessage botUsername { Message = message; ReplyToMessage = reply } with
    | Some replyMessage ->
        Assert.Equal(replyMessage.BotUsername, "@bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(newUser, replyMessage.ReplyToUser)
        Assert.Equal(replyMessage.Message, message)
    | None ->
        Assert.Fail()
        
[<Theory>]
[<ClassData(typeof<NotValidReplyMessageGenerator>)>]
let ``prepareReplyToMessage returns None``(message: ReplyMessage) =
    let botUsername = Some "bot"
    match prepareReplyToMessage botUsername message with
    | Some _ ->
        Assert.Fail()
    | None ->
        Assert.Success()
        
[<Fact>]
let ``prepareReplyToMessage returns None when bot username is None``() =
    match prepareReplyToMessage None { Message = defaultMessage; ReplyToMessage = defaultMessage } with
    | Some _ ->
        Assert.Fail()
    | None ->
        Assert.Success()
        
[<Fact>]
let ``authorize returns CommandAllowed``() =
    let settings = createSettings [|"chat"|] [|"user"|]
    
    let message = { defaultTextMessage with FromUsername = "user"; ChatUsername = "chat" }
    
    match authorize settings message.FromUsername message.ChatUsername with
    | CommandAllowed ->
        Assert.Success()
    | CommandNotAllowed ->
        Assert.Fail()
        
[<Theory>]
[<InlineData(null, null)>]
[<InlineData("chat", null)>]
[<InlineData(null, "user")>]
[<InlineData("chat1", "user")>]
[<InlineData("chat", "user1")>]
let ``authorize returns CommandNotAllowed``(chat: string, user: string) =
    let settings = createSettings [|chat|] [|user|]
    
    let message = { defaultTextMessage with FromUsername = "user"; ChatUsername = "chat" }
    
    match authorize settings message.FromUsername message.ChatUsername with
    | CommandAllowed ->
        Assert.Fail()
    | CommandNotAllowed ->
        Assert.Success()
        
[<Fact>]
let ``BanOnReplyMessage FormatAsString returns correct message``() =
    let expected = """Banned 1 (user) in chats @chat1, @chat2 forever"""
    
    let message = {
      Username = "user"
      UserId = 1L
      Chats = Set.ofSeq [|"@chat1"; "@chat2"|]
    }
    
    let messageText = (message :> IMessage).FormatAsString()
    
    Assert.Equal(expected, messageText)
    
[<Fact>]
let ``BanMessage FormatAsString returns correct message when date is less than a year ahead``() =
    let expected = """Banned @user1, @user2 in chats @chat1, @chat2 until 2017-01-01 02:02:02 UTC"""
    
    let message = {
      Usernames = [|"@user1"; "@user2"|]
      Until = DateTime(2017,1,1,2,2,2) |> Timed
      Chats = Set.ofSeq [|"@chat1"; "@chat2"|]
    }
    
    let messageText = (message :> IMessage).FormatAsString()
    
    Assert.Equal(expected, messageText)

    
[<Fact>]
let ``BanMessage FormatAsString returns correct message when date is more than a year ahead``() =
    let expected = """Banned @user1, @user2 in chats @chat1, @chat2 forever"""
    
    let message = {
      Usernames = [|"@user1"; "@user2"|]
      Until = DateTime.UtcNow.AddMonths(13) |> Timed
      Chats = Set.ofSeq ["@chat1"; "@chat2"]
    }
    
    let messageText = (message :> IMessage).FormatAsString()
    
    Assert.Equal(expected, messageText)
    
[<Fact>]
let ``UnbanMessage FormatAsString returns correct message``() =
    let expected = """Unbanned @user1, @user2 in chats @chat1, @chat2"""
    
    let message = {
      Usernames = [|"@user1"; "@user2"|]
      Chats = Set.ofSeq ["@chat1"; "@chat2"]
    }
    
    let messageText = (message :> IMessage).FormatAsString()
    
    Assert.Equal(expected, messageText)
    
[<Fact>]
let ``formatMessage returns correct message for ban message``() = async {
    let message = {
      Usernames = [|"@user1"; "@user2"|]
      Until = DateTime(2017,1,1,2,2,2) |> Timed
      Chats = Set.ofSeq ["@chat1"; "@chat2"]
    }
    
    let expected = "Ban command from: @user\n\nBanned @user1, @user2 in chats @chat1, @chat2 until 2017-01-01 02:02:02 UTC\n\nApi error"
    
    let commandMessage = BanMessage("user", message, [|ApiError("Api error")|])
    
    Assert.Equal(expected, formatMessage commandMessage)
}
    
[<Fact>]
let ``formatMessage returns correct message for ban on reply message``() = async {
    let message = {
      UserId = 1L
      Username = "@user1"
      Chats = Set.ofSeq ["@chat1"; "@chat2"]
    }
    
    let expected = "Ban on reply command from: @user\n\nBanned 1 (@user1) in chats @chat1, @chat2 forever\n\nApi error"
    
    let commandMessage = BanOnReplyMessage("user", message, [|ApiError("Api error")|])
    
    Assert.Equal(expected, formatMessage commandMessage)
}

[<Fact>]
let ``formatMessage returns correct message for ban on reply message without errors``() = async {
    let message = {
      UserId = 1L
      Username = "@user1"
      Chats = Set.ofSeq ["@chat1"; "@chat2"]
    }
    
    let expected = "Ban on reply command from: @user\n\nBanned 1 (@user1) in chats @chat1, @chat2 forever"
    
    let commandMessage = BanOnReplyMessage("user", message, [||])
    
    Assert.Equal(expected, formatMessage commandMessage)
}

[<Fact>]
let ``formatMessage returns correct message for unban``() = async {
    let message = {
      Usernames = [|"@user1"; "@user2"|]
      Chats = Set.ofSeq ["@chat1"; "@chat2"]
    }
    
    let expected = "Unban command from: @user\n\nUnbanned @user1, @user2 in chats @chat1, @chat2"
    
    let commandMessage = UnbanMessage("user", message, [||])
    
    Assert.Equal(expected, formatMessage commandMessage)
}