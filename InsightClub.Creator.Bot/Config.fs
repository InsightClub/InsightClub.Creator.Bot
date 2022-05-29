module InsightClub.Creator.Bot.Config

open System


type Config =
  { BotToken: String
    BotAddress: String
    BotEndPoint: String
    DatabaseUrl: String
    DropboxAccessToken: String }

module Config =
  let private get variable =
    Environment.GetEnvironmentVariable variable

  let load () =
    { BotToken = get "BOT_TOKEN"
      BotAddress = get "BOT_ADDRESS"
      BotEndPoint = get "BOT_ENDPOINT"
      DatabaseUrl = get "DATABASE_URL"
      DropboxAccessToken = get "DROPBOX_ACCESS_TOKEN" }
