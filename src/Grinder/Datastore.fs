namespace Grinder

open System
open Hopac
open Microsoft.EntityFrameworkCore
open Grinder.DataAccess
open System.Linq
open FSharp.Control
    
type FindUserIdByUsernameResult =
  | UserIdNotFound
  | UserIdFound of int64
        
type FindUsernameByUserIdResult =
  | UsernameNotFound
  | UsernameFound of string

[<RequireQualifiedAccess>]
module Datastore =
  let upsertUsers (users: User seq) =
    job {
      use context = new GrinderContext()
      do! Job.fromUnitTask (fun () -> context.Users.AddOrUpdateUsers(users))
      do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }
  
  let findUserIdByUsername (username: string) =
    job {
      use context = new GrinderContext()
      let! user =
        context.Users.AsNoTracking()
          .FirstOrDefaultAsync(fun u -> u.Username = username.TrimStart('@'))
      return 
        user
        |> Option.ofObj
        |> Option.fold (fun _ u -> UserIdFound u.UserId) UserIdNotFound
    }

  let findUsernameByUserId userId =
    job {
      use context = new GrinderContext()
      let! user =
        context.Users.AsNoTracking()
          .FirstOrDefaultAsync(fun u -> u.UserId = userId)
      
      return 
        user
        |> Option.ofObj
        |> Option.fold (fun _ u -> UsernameFound u.Username) UsernameNotFound
    }

  let getUsernameByUserId userId = 
    job {
      match! findUsernameByUserId userId with
      | UsernameFound username ->
        return Some (sprintf "@%s" username)
      | UsernameNotFound ->
        return None
    }

  let insertMessages (messages: seq<_>) = 
    job {
      use context = new GrinderContext()
      do! Job.fromUnitTask (fun () -> context.Set<Message>().AddRangeAsync(messages))
      do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }

  let getLastThreeMessagesInChats userId =
    job {
      use context = new GrinderContext()
      let takeUpToThree =
        let mutable count = 0
        fun _ ->
          count <- count + 1
          count < 3

      let! messages = 
        Job.fromTask(fun () ->
          context.Messages.AsNoTracking()
            .Where(fun m -> m.UserId = userId)
            .GroupBy(fun m -> m.ChatId)
            .Select(fun g -> 
              let messages =
                g.OrderByDescending(fun m -> m.Date)
                 .TakeWhile(takeUpToThree)
                 .ToArray()
              {| ChatId = g.Key; Messages = messages |}
            )
            .ToArrayAsync()
        )
      return messages
    }

  let startMessageCleanupJob() =
    job {
      use context = new DataAccess.GrinderContext()
      let! toDelete =
        Job.fromTask(fun () ->
          context.Messages.AsNoTracking()
            .Where(fun m -> 
              DateTimeOffset.UtcNow.Subtract(DateTimeOffset.FromUnixTimeMilliseconds(m.Date)).TotalDays > 1.
            )
            .ToArrayAsync()
        )
      context.Messages.RemoveRange(toDelete)
      do! Job.fromTask(fun () -> context.SaveChangesAsync()) |> Job.Ignore
      do! timeOut (TimeSpan.FromDays(14.))
    }
    |> Job.forever
    |> queue

  let getAdminUsersAndChatsToMonitor() =
    job {
      use context = new DataAccess.GrinderContext()
      let! chats = context.ChatsToMonitor.AsNoTracking().Select(fun x -> x.Username).ToArrayAsync()
      let! users = context.AdminUsers.AsNoTracking().Select(fun x -> x.Username).ToArrayAsync()
      return users, chats
    }

  let addAdminUser user =
    job {
      use context = new DataAccess.GrinderContext()
      let _ = context.AdminUsers.Add(new AdminUser(Username = user))
      do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }

  let addChatToMonitor user =
    job {
      use context = new DataAccess.GrinderContext()
      let _ = context.ChatsToMonitor.Add(new ChatToMonitor(Username = user))
      do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }

  let removeAdminUser user =
    job {
      use context = new DataAccess.GrinderContext()
      let! userFromDb = context.AdminUsers.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Username = user)
      match userFromDb with
      | null -> ()
      | userFromDb ->
        let _ = context.AdminUsers.Remove(userFromDb)
        do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }

  let removeChatToMonitor chat =
    job {
      use context = new DataAccess.GrinderContext()
      let! chatFromDb = context.ChatsToMonitor.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Username = chat)
      match chatFromDb with
      | null -> ()
      | chatFromDb ->
        let _ = context.ChatsToMonitor.Remove(chatFromDb)
        do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }