namespace Grinder

open FSharp.Control.Tasks.V2
open Microsoft.EntityFrameworkCore
open Grinder.DataAccess
    
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
        task {
            use context = new GrinderContext()
            let! user =
                context.Users
                    .FirstOrDefaultAsync(fun u -> u.Username = username)
            return user
                   |> Option.ofObj
                   |> Option.fold (fun _ u -> UserIdFound u.UserId) UserIdNotFound
        }
        |> Async.AwaitTask
        
    