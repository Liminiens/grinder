namespace Grinder

open Funogram.Telegram.Types
open Xunit

[<RequireQualifiedAccess>]    
module Assert =
  let inline Fail() = Assert.True(false)
  
  let inline FailWithMessage text = Assert.True(false, text)
  
  let inline Success() = Assert.True(true)
    
[<AutoOpen>]
module Funogram =
  let defaultUser =
     { Id = -1L; IsBot = false; FirstName = null; LastName = None; Username = None; LanguageCode = None }

[<AutoOpen>]         
module App =
  open System
  open Grinder.Types
  open Grinder.Commands.Processing
  
  let defaultTextMessage =
    { BotUsername = String.Empty
      Message = defaultMessage
      MessageText = String.Empty
      ReplyToUser = defaultUser
      ReplyToMessage = defaultMessage
      FromUsername = String.Empty
      ChatUsername = String.Empty }
      
  let createSettings chats users = {
    ChatsToMonitor = Set.ofSeq chats
    AllowedUsers = Set.ofSeq users
    ChannelId = 123L
    AdminUserId = 123L
  }