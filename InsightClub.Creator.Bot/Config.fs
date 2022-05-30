module InsightClub.Creator.Bot.Config

open System


type Config =
  { BotToken: String
    WebhookAddress: String
    BotEndPoint: String
    DatabaseUrl: String
    DropboxAccessToken: String }

module Config =
  let private get variable =
    Environment.GetEnvironmentVariable variable

  let load () =
    { BotToken = get "BOT_TOKEN"
      WebhookAddress = get "WEBHOOK_ADDRESS"
      BotEndPoint = $"""http://*:{get "PORT"}/"""
      DatabaseUrl = get "DATABASE_URL"
      DropboxAccessToken = get "DROPBOX_ACCESS_TOKEN" }
