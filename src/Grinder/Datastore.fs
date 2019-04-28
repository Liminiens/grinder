namespace Grinder

open System
open FSharp.Control.Tasks.V2
open Grinder.DataAccess
   
type BanStateChange =
    | BanLifted
    | Banned of DateTimeOffset
    
type FindUserIdByUsernameResult =
    | UserIdNotFound
    | UserIdFound of int64
        
[<RequireQualifiedAccess>]
module Datastore =
    let upsertUsers (users: User seq) =
        task {
            use context = new GrinderContext()
            do! context.Users.AddOrUpdateUsers(users)
            do! context.SaveChangesAsync() |> Task.Ignore
        }
        |> Async.AwaitTask
    
    let findUserIdByUsername username =
        use context = new GrinderContext()
        query {
            for user in context.Users do
                where (user.Username = username)
                select (toNullable user.UserId)
                exactlyOneOrDefault
        }
        |> Option.ofNullable
        |> Option.fold (fun _ v -> UserIdFound v) UserIdNotFound
        
    let setBanDuration userid duration =
        task {
            use context = new GrinderContext()
            let userToUpdate = 
                query {
                    for user in context.Users do
                        where (user.UserId = userid)
                        select user
                        exactlyOne
                }
                
            match duration with
            | Banned untill ->
                userToUpdate.BannedUntil <- toNullable untill
            | BanLifted -> 
                userToUpdate.BannedUntil <- Nullable()
                
            context.Attach(userToUpdate).Property("BannedUntil").IsModified <- true
            do! context.SaveChangesAsync() |> Task.Ignore
        }
        |> Async.AwaitTask
        
    