namespace Grinder

open FSharp.Control.Tasks.V2
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
        task {
            try
                for chunk in Seq.chunkBySize 500 users do
                    use context = new GrinderContext()
                    do! context.Users.AddOrUpdateUsers(chunk)
                    do! context.SaveChangesAsync() |> Task.Ignore
            with e ->
                sprintf "Error on upserting new users %A" users
                |> logExn e
        }
        |> Async.AwaitTask
    
    let findUserIdByUsername (username: string) =
        task {
            use context = new GrinderContext()
            let! user =
                context.Users
                    .FirstOrDefaultAsync(fun u -> u.Username = username.TrimStart('@'))
            return user
                   |> Option.ofObj
                   |> Option.fold (fun _ u -> UserIdFound u.UserId) UserIdNotFound
        }
        |> Async.AwaitTask

    let findUsernameByUserId userId =
        task {
            use context = new GrinderContext()
            let! user =
                context.Users
                    .FirstOrDefaultAsync(fun u -> u.UserId = userId)
            
            return user
                   |> Option.ofObj
                   |> Option.fold (fun _ u -> UsernameFound u.Username) UsernameNotFound
        }
        |> Async.AwaitTask
        
open Grinder.Types

type IDataAccessApi =
    abstract member GetUsernameByUserId: TelegramUserId -> Async<UserUsername option>
    abstract member UpsertUsers: User seq -> Async<unit>