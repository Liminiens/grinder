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
    IVar.fill botConfig config
    |> start

    let settings = {
      ChatsToMonitor = Set.ofArray config.DefaultChatsToMonitor
      AllowedUsers = Set.ofArray config.DefaultAllowedUsers
      ChannelId = config.ChannelId
      AdminUserId = config.AdminUserId
    }

    MVar.fill botSettings settings |> start
  
  let getCurrentSettings() = MVar.read botSettings