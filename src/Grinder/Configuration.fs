namespace Grinder

open Hopac

type BotSettings = {
  ChatsToMonitor: Set<string>
  AllowedUsers: Set<string>
  ChannelId: int64
  AdminUserId: int64
}

[<CLIMutable>]
type Socks5Configuration = {
  Hostname: string
  Port: int
  Username: string
  Password: string
}

[<CLIMutable>]
type BotConfig = {
  Socks5Proxy: Socks5Configuration
  Token: string
  DefaultChatsToMonitor: string array
  DefaultAllowedUsers: string array
  ChannelId: int64
  AdminUserId: int64
}

[<CLIMutable>]
type Config = {
  Bot: BotConfig
}

[<RequireQualifiedAccess>]
module Configuration =
  let private botConfig = IVar<BotConfig>()
  let private botSettings = MVar<BotSettings>()

  let setBotConfig config =
    job {
      do! IVar.fill botConfig config
      
      let! users, chats = Datastore.getAdminUsersAndChatsToMonitor()

      let settings = {
        ChatsToMonitor = Set.ofSeq (config.DefaultChatsToMonitor |> Seq.append chats)
        AllowedUsers = Set.ofSeq (config.DefaultAllowedUsers |> Seq.append users)
        ChannelId = config.ChannelId
        AdminUserId = config.AdminUserId
      }
      
      do! MVar.fill botSettings settings
    } 
    |> start

  let getCurrentSettings() = MVar.read botSettings

  let addAdminUser user =
    job {
      let! settings = MVar.read botSettings
      let! config = IVar.read botConfig
      let newUsers =
        config.DefaultAllowedUsers
        |> Seq.append settings.AllowedUsers
        |> Seq.append (Seq.singleton user)
        |> Set.ofSeq
      let newSettings = { settings with AllowedUsers = newUsers }
      do! MVar.fill botSettings newSettings
    }

  let addChatToMonitor chat =
    job {
      let! settings = MVar.read botSettings
      let! config = IVar.read botConfig
      let newChats =
        config.DefaultChatsToMonitor
        |> Seq.append settings.ChatsToMonitor
        |> Seq.append (Seq.singleton chat)
        |> Set.ofSeq
      let newSettings = { settings with ChatsToMonitor = newChats }
      do! MVar.fill botSettings newSettings
    }

  let removeAdminUser user =
    job {
      let! settings = MVar.read botSettings
      let! config = IVar.read botConfig
      let newUsers =
        config.DefaultAllowedUsers
        |> Seq.append (Set.remove user settings.AllowedUsers)
        |> Set.ofSeq
      let newSettings = { settings with AllowedUsers = newUsers }
      do! MVar.fill botSettings newSettings
    }

  let removeChatToMonitor chat =
    job {
      let! settings = MVar.read botSettings
      let! config = IVar.read botConfig
      let newChats =
        config.DefaultChatsToMonitor
        |> Seq.append (Set.remove chat settings.ChatsToMonitor)
        |> Set.ofSeq
      let newSettings = { settings with ChatsToMonitor = newChats }
      do! MVar.fill botSettings newSettings
    }