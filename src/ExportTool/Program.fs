open FSharp.Control
open FSharp.Control.Tasks.V2
open Newtonsoft.Json
open System.IO
open System
open System.Threading
open System.Threading.Tasks
open TdLib

let inline (^) f x = f x

let inline retype (x: 'a) : 'b = (# "" x : 'b #)

[<RequireQualifiedAccess>]
module Observable =
    let ofType<'a, 'b> (obs: IObservable<'a>): IObservable<'b> =
        obs
        |> Observable.filter ^ fun x ->
            typeof<'b>.IsAssignableFrom(x.GetType())
        |> Observable.map ^ fun x ->
            retype x    

[<RequireQualifiedAccess>]
module AsyncSeq =
    let mapTask fn aseq =
        aseq
        |> AsyncSeq.mapAsync ^ fun x ->
            fn x |> Async.AwaitTask
    
    let ofType<'a, 'b> (aseq: AsyncSeq<'a>): AsyncSeq<'b> =
        aseq
        |> AsyncSeq.filter ^ fun x ->
            typeof<'b>.IsAssignableFrom(x.GetType())
        |> AsyncSeq.map ^ fun x ->
            retype x
    
    let mapTaskParallel fn aseq =
        aseq
        |> AsyncSeq.mapAsyncParallel ^ fun x ->
            fn x |> Async.AwaitTask
        
[<RequireQualifiedAccess>]
module Task =
    let inline Ignore (t: Task<_>) = t :> Task
    
let updateAuthorizationState (dialer: Dialer) (authLock: AutoResetEvent) (state: TdApi.Update.UpdateAuthorizationState) =
    task {
        match state.AuthorizationState with
        | :? TdApi.AuthorizationState.AuthorizationStateWaitTdlibParameters ->
            let parameters = new TdApi.TdlibParameters()
            parameters.DatabaseDirectory <- "tdlib"
            parameters.UseFileDatabase <- false
            parameters.UseMessageDatabase <- false
            parameters.UseChatInfoDatabase <- false
            parameters.UseSecretChats <- true
            parameters.ApiId <- 835868
            parameters.ApiHash <- "82323bf163ad3d0b544c3de856282f18"
            parameters.SystemLanguageCode <- "en"
            parameters.DeviceModel <- "Desktop"
            parameters.SystemVersion <- "Unknown"
            parameters.ApplicationVersion <- "1.0"
            parameters.EnableStorageOptimizer <- true
            
            do! dialer.ExecuteAsync(new TdApi.SetTdlibParameters(Parameters = parameters))
                |> Task.Ignore
                
        | :? TdApi.AuthorizationState.AuthorizationStateWaitEncryptionKey ->
            do! dialer.ExecuteAsync(new TdApi.CheckDatabaseEncryptionKey())
                |> Task.Ignore
                
        | :? TdApi.AuthorizationState.AuthorizationStateWaitPhoneNumber ->
            printfn "Enter phone"
            let phone = Console.ReadLine()
            do! dialer.ExecuteAsync(new TdApi.SetAuthenticationPhoneNumber(PhoneNumber = phone))
                |> Task.Ignore
                
        | :? TdApi.AuthorizationState.AuthorizationStateWaitCode ->
            printfn "Enter code"
            let code = Console.ReadLine()
            do! dialer.ExecuteAsync(new TdApi.CheckAuthenticationCode(Code = code))
                |> Task.Ignore
                
        | :? TdApi.AuthorizationState.AuthorizationStateReady  ->
            authLock.Set() |> ignore
        | _  ->
            ()
    }
    |> Async.AwaitTask
    |> Async.Start

let getAllSupergroupMembers (dialer: Dialer) supergroupId = task {
    let! info = dialer.ExecuteAsync(new TdApi.GetSupergroupFullInfo(SupergroupId = supergroupId))
    let memberCount = info.MemberCount
    let rec call result = task {
        let currentMemberCount = List.length result
        match currentMemberCount with
        | x when x = memberCount ->
            return result
        | _ ->
            let! groupMembers =
                dialer.ExecuteAsync(new TdApi.GetSupergroupMembers(SupergroupId = supergroupId, Offset = currentMemberCount, Limit = 200))
            let members = groupMembers.Members |> List.ofArray
            let total = members @ result
            if List.length total < memberCount then
                return! call total
            else
                return total
    }
    return! call []
}

type Username = { UserId: int; Username: string }

[<EntryPoint>]
let main argv =
    Client.Log.SetVerbosityLevel(1)
    
    use client = new Client()
    let hub = new Hub(client)
    let dialer = new Dialer(client, hub)
    let _ = Task.Run(fun _ -> hub.Start())
    
    let updates =
        hub.Received
        |> Observable.ofType<_, TdApi.Update>
    
    let authLock = new AutoResetEvent(false)

    use authUpdates =
        updates
        |> Observable.ofType<_, TdApi.Update.UpdateAuthorizationState>
        |> Observable.subscribe (updateAuthorizationState dialer authLock)
        
    async {
        do! dialer.ExecuteAsync(new TdApi.GetTextEntities())
            |> Async.AwaitTask
            |> Async.Ignore
        
        while not <| authLock.WaitOne() do ()
        
        let chatNames =
            [ "fsharp_flood"; "fsharp_chat"; "pro_net"; "DotNetRuChat"; "dotnettalks";
              "DotNetChat"; "microsoftstackjobs"; "powershell_pro"; "cilchat" ]
        
        let! chats =
            dialer.ExecuteAsync(new TdApi.GetChats(Limit = 200, OffsetOrder = Int64.MaxValue))
            |> Async.AwaitTask
        
        let! users =
            chats.ChatIds
            |> AsyncSeq.ofSeq
            |> AsyncSeq.mapTask ^ fun chatId -> task {
                let! chat = dialer.ExecuteAsync(new TdApi.GetChat(ChatId = chatId))
                return chat.Type
            }
            |> AsyncSeq.ofType<_, TdApi.ChatType.ChatTypeSupergroup>
            |> AsyncSeq.mapTask ^ fun supergroupType ->
                dialer.ExecuteAsync(new TdApi.GetSupergroup(SupergroupId = supergroupType.SupergroupId))
            |> AsyncSeq.filter ^ fun supergroup ->
                chatNames
                |> List.contains supergroup.Username
            |> AsyncSeq.mapTaskParallel ^ fun supergroup ->
                getAllSupergroupMembers dialer supergroup.Id
            |> AsyncSeq.collect ^ fun members ->
                members
                |> AsyncSeq.ofSeq
            |> AsyncSeq.mapTask ^ fun superGroupMember ->
                dialer.ExecuteAsync(new TdApi.GetUser(UserId = superGroupMember.UserId))
            |> AsyncSeq.filter ^ fun user ->
                not <| String.IsNullOrWhiteSpace(user.Username)
            |> AsyncSeq.map ^ fun user ->
                { UserId = user.Id; Username = user.Username }
            |> AsyncSeq.toListAsync
            
        do
            use stream =
                let file =
                    Path.Combine(Directory.GetCurrentDirectory(), sprintf "%s.json" (Guid.NewGuid().ToString()))
                    |> File.Create
                new StreamWriter(file)
            users |> JsonConvert.SerializeObject |> stream.Write  
        do! Async.Sleep(-1)
    } |> Async.RunSynchronously
    0 // return an integer exit code
