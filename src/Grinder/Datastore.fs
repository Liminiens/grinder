namespace Grinder

open Hopac
open Microsoft.EntityFrameworkCore
open Grinder.DataAccess
open System
open System.Linq
    
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
      let! idsFromDb = 
        Job.fromTask (fun () ->
          context.Messages
            .Where(fun m -> m.ChatId = chatId && m.UserId = userId)
            .OrderByDescending(fun m -> m.Date)
            .ToArrayAsync()
        )
      let ids =
        match idsFromDb with
        | ids when ids.Length > 3 ->
          Seq.take 3 ids 
          |> Seq.map (fun m -> m.MessageId)
          |> Array.ofSeq
        | ids -> 
          ids 
          |> Array.map (fun m -> m.MessageId)
      return ids
    }