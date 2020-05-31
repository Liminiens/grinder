namespace Grinder

open Hopac
open Microsoft.EntityFrameworkCore
open Grinder.DataAccess
    
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