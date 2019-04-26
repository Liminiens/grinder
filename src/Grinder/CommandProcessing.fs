namespace Grinder

open Funogram
open Funogram.Bot
open Funogram.Types
open Control
open System

module CommandProcessing =
    
    type UserMessageContext = {
        UpdateContext: UpdateContext
        BotUsername: string
        Message: Message
        MessageText: string
        From: User
        FromUsername: string
        ChatUsername: string
    }
    
    let iterTextMessage fn (context: UpdateContext) (message: Message) =
        context.Me.Username
        |> Option.bind ^ fun botUsername ->
            message.From
            |> Option.map ^ fun from ->
                (botUsername, message, from)
        |> Option.bind ^ fun (botUsername, message, from) ->
            from.Username
            |> Option.map ^ fun username ->
                (botUsername, message, from, username)
        |> Option.bind ^ fun (botUsername, message, from, username) ->
            message.Text
            |> Option.map ^ fun text ->
                (botUsername, message, from, username, text)
        |> Option.bind ^ fun (botUsername, message, from, username, text) ->
            message.Chat.Username
            |> Option.map ^ fun chatUsername -> {
                UpdateContext = context
                BotUsername = botUsername
                Message = message
                MessageText = text
                From = from
                FromUsername = username
                ChatUsername = chatUsername
            }
        |> Option.iter fn
    
    let private (|CommandAllowed|CommandNotAllowed|) (allowedUsers, chatsToMonitor, username: string, chatUsername: string) =
        let isAllowedUser username =
            allowedUsers 
            |> Array.contains username

        let isAllowedChat chatUsername =
            chatsToMonitor 
            |> Array.contains chatUsername
            
        if isAllowedUser username && isAllowedChat chatUsername then    
            CommandAllowed
        else
            CommandNotAllowed
            
    let processTextCommand (context: UserMessageContext) allowedUsers chatsToMonitor reportChannelId = async {
        match (allowedUsers, chatsToMonitor, context.FromUsername, context.ChatUsername) with
        | CommandAllowed ->
            do! Api.deleteMessage context.Message.Chat.Id context.Message.MessageId
                |> callApiWithDefaultRetry context.UpdateContext
                |> Async.Ignore
            
            let parsedMessage = Control.parse context.BotUsername context.MessageText
            let requests = [
                for chat in chatsToMonitor do
                    match parsedMessage with
                    | Ban(UsernameList usernames, time) ->
                        yield!
                            usernames
                            |> Seq.map ^ fun user ->
                                ApiExt.restrictUser context.UpdateContext chat user time
                    | Unban(UsernameList usernames) ->
                        yield!
                            usernames
                            |> Seq.map ^ fun user ->
                                ApiExt.unrestrictUser context.UpdateContext chat user
                    | IgnoreCommand ->
                        ()
            ]
            let! text = Async.Parallel requests
            do! String.Join('\n', text)
                |> sprintf "Username: %s\n\n%s" context.FromUsername
                |> ApiExt.sendMessage reportChannelId context.UpdateContext
        | CommandNotAllowed ->
            ()        
    }

