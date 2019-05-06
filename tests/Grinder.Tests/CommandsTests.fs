module Grinder.CommandsTests

open System
open System.Collections
open System.Collections.Generic
open Funogram.Types
open Grinder.Commands.Processing
open Grinder.Types
open FSharp.UMX
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
                                with NewChatMember = None
                                     From =  None } }
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  Some { defaultUser with Username = Some "user" } };
                          ReplyToMessage =
                              { defaultMessage
                                with NewChatMember = None
                                     From =  None } }
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  None };
                          ReplyToMessage =
                              { defaultMessage
                                with NewChatMember = None
                                     From =  Some { defaultUser with Username = Some "user" } } }
                    yield
                        { Message =
                            { defaultMessage
                                with Chat = { defaultChat with Username = Some "chat" }
                                     From =  None };
                          ReplyToMessage =
                              { defaultMessage
                                with NewChatMember = Some { defaultUser with Username = Some "user" } } }
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
        Assert.Equal(textMessage.BotUsername, "bot")
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
        Assert.Equal(replyMessage.BotUsername, "bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(replyMessage.ReplyToUser, replyToUser)
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
                 NewChatMember = Some newUser }
        
    match prepareReplyToMessage botUsername { Message = message; ReplyToMessage = reply } with
    | Some replyMessage ->
        Assert.Equal(replyMessage.BotUsername, "bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(replyMessage.ReplyToUser, newUser)
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
            with NewChatMember = Some newUser }
        
    match prepareReplyToMessage botUsername { Message = message; ReplyToMessage = reply } with
    | Some replyMessage ->
        Assert.Equal(replyMessage.BotUsername, "bot")
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
        Assert.Equal(replyMessage.BotUsername, "bot")
        Assert.Equal(replyMessage.ChatUsername, "@chat")
        Assert.Equal(replyMessage.FromUsername, "user")
        Assert.Equal(replyMessage.MessageText, "text")
        Assert.Equal(replyMessage.ReplyToUser, newUser)
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
    
    let message = { defaultTextMessage with FromUsername = %"user"; ChatUsername = %"chat" }
    
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
    
    let message = { defaultTextMessage with FromUsername = %"user"; ChatUsername = %"chat" }
    
    match authorize settings message.FromUsername message.ChatUsername with
    | CommandAllowed ->
        Assert.Fail()
    | CommandNotAllowed ->
        Assert.Success()