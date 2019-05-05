module Grinder.CommandsTests

open System.Collections
open System.Collections.Generic
open Funogram.Types
open Grinder.Commands.Processing
open Xunit

let defaultUser =
     { Id = -1L; IsBot = false; FirstName = null; LastName = None; Username = None; LanguageCode = None }

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