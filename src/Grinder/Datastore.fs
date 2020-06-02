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
      for chunk in Seq.chunkBySize 500 users do
        use context = new GrinderContext()
        do! Job.fromUnitTask (fun () -> context.Users.AddOrUpdateUsers(chunk))
        do! Job.fromTask (fun () -> context.SaveChangesAsync()) |> Job.Ignore
    }
  
  let findUserIdByUsername (username: string) =
    job {
      use context = new GrinderContext()
      let! user =
        context.Users
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
        context.Users
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

  let getLastThreeMessages chatId userId =
    job {
      use context = new GrinderContext()
      let takeUpToThree =
        let mutable count = 0
        fun _ ->
          count <- count + 1
          count < 3 |> async.Return

      let! ids = 
        context.Messages
          .Where(fun m -> m.ChatId = chatId && m.UserId = userId)
          .OrderByDescending(fun m -> m.Date)
        |> AsyncSeq.ofIQueryable
        |> AsyncSeq.takeWhileAsync takeUpToThree
        |> AsyncSeq.mapAsync (fun m -> m.MessageId |> async.Return)
        |> AsyncSeq.toArrayAsync
        |> Job.fromAsync
      return ids
    }

  let startMessageCleanupJob() =
    job {
      use context = new DataAccess.GrinderContext()
      let! toDelete =
        Job.fromTask(fun () ->
          context.Messages
            .Where(fun m -> 
              DateTimeOffset.UtcNow.Subtract(DateTimeOffset.FromUnixTimeMilliseconds(m.Date)).TotalDays > 1.
            )
            .ToArrayAsync()
        )
      context.Messages.RemoveRange(toDelete)
      do! Job.fromTask(fun () -> context.SaveChangesAsync()) |> Job.Ignore
      do! timeOut (TimeSpan.FromDays(1.))
    }
    |> Job.forever
    |> queue