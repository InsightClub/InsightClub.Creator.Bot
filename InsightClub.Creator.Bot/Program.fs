module InsightClub.Creator.Bot.Program

open Config
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Npgsql
open Npgsql.FSharp
open System.Net
open System


let startBot
  (appConfig: Config)
  (listener: HttpListener)
  (getConnection: unit -> NpgsqlConnection) =
  let apiPath = $"api/{appConfig.BotToken}"

  let webhookUrl =
    appConfig.WebhookAddress + apiPath

  let validate (req: HttpListenerRequest) =
    req.Url.LocalPath = $"/{apiPath}"

  let webhook =
    { Listener = listener
      ValidateRequest = validate }

  let botConfig =
    { defaultConfig with
        Token = appConfig.BotToken
        WebHook = Some webhook }

  let printError e =
    sprintf "Failed creating webhook on %s: %A" webhookUrl e
    |> failwith

  let printStarted () =
    printfn
      "Bot started! Listening to %s"
      appConfig.BotEndPoint

  let setWebhook () =
    setWebhookBase webhookUrl None None None
    |> api botConfig
    |> Async.map (Result.mapError printError)
    |> Async.Ignore

  let startBot () =
    printStarted ()
    startBot botConfig (Api.onUpdate getConnection appConfig.DropboxAccessToken) None

  async {
    do! setWebhook ()
    do! startBot () }
  |> Async.Ignore

[<EntryPoint>]
let main _ =
  let config = Config.load ()

  use listener = new HttpListener()
  listener.Prefixes.Add(config.BotEndPoint)

  let getConnection () =
    config.DatabaseUrl
    |> Uri
    |> Sql.fromUriToConfig
    |> Sql.sslMode SslMode.Require
    |> Sql.trustServerCertificate true
    |> Sql.formatConnectionString
    |> Sql.connect
    |> Sql.createConnection

  // Test connection
  using (getConnection()) (fun c -> c.Open())

  // Run synchronously to block the tread
  // Don't use Async.StartImmediate or the
  // program will immediately shut after the launch
  startBot config listener getConnection
  |> Async.Ignore
  |> Async.RunSynchronously

  0 // Return an integer exit code
