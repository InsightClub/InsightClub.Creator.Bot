module InsightClub.Creator.Bot.Config

open System


type Config =
  { BotToken: String
    WebhookAddress: String
    BotEndPoint: String
    DatabaseUrl: String
    FilebaseAccessKey: String
    FilebaseSecretKey: String
    FilebaseBucketName: String }

module Config =
  let private get variable =
    Environment.GetEnvironmentVariable variable

  let load () =
    { BotToken = get "BOT_TOKEN"
      WebhookAddress = get "WEBHOOK_ADDRESS"
      BotEndPoint = $"""http://*:{get "PORT"}/"""
      DatabaseUrl = get "DATABASE_URL"
      FilebaseAccessKey = get "FILEBASE_ACCESS_KEY"
      FilebaseSecretKey = get "FILEBASE_SECRET_KEY"
      FilebaseBucketName = get "FILEBASE_BUCKET_NAME" }
