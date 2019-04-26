namespace Grinder

open System
open Grinder.DataAccess
   
type BanStateChange =
    | BanLifted
    | Banned of DateTimeOffset
    
type FindUserIdByUsernameResult =
    | UserIdNotFound
    | UserIdFound of int64
        
[<RequireQualifiedAccess>]
module Datastore =    
    let addUsers (users: User list) =
        use context = new GrinderContext()
        context.AddRange(users)
        context.SaveChanges() |> ignore
    
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
        context.SaveChanges() |> ignore
    