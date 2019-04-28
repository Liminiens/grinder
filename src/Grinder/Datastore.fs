namespace Grinder

open FSharp.Control.Tasks.V2
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
        use context = new GrinderContext()
        query {
            for user in context.Users do
                where (user.Username = username)
                select (toNullable user.UserId)
                exactlyOneOrDefault
        }
        |> Option.ofNullable
        |> Option.fold (fun _ v -> UserIdFound v) UserIdNotFound
        
    