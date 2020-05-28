namespace Grinder

open Funogram.Telegram.Types
open Xunit

[<RequireQualifiedAccess>]    
module Assert =
    let Fail() = Assert.True(false)
    
    let FailWithMessage text = Assert.True(false, text)
    
    let Success() = Assert.True(true)
    
[<AutoOpen>]
module Funogram =
    let defaultUser =
         { Id = -1L; IsBot = false; FirstName = null; LastName = None; Username = None; LanguageCode = None }

[<AutoOpen>]         
module App =
    open System
    open FSharp.UMX
    open Grinder.Types
    open Grinder.Commands.Processing
    
    let defaultTextMessage =
        { BotUsername = %String.Empty
          Message = defaultMessage
          MessageText = String.Empty
          ReplyToUser = Seq.singleton defaultUser
          ReplyToMessage = defaultMessage
          FromUsername = %String.Empty
          ChatUsername = %String.Empty }
        
    let createSettings chats users = {
        Token = String.Empty
        ChatsToMonitor = ChatsToMonitor.Create chats
        AllowedUsers = AllowedUsers.Create users
        ChannelId = %123L
        AdminUserId = %123L
    }