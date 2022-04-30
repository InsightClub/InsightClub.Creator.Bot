module InsightClub.Creator.Bot.Program

open Config
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Npgsql
open Npgsql.FSharp
open System.Net


let startBot
  (appConfig: Config)
  (listener: HttpListener)
  (getConnection: unit -> NpgsqlConnection) =
  let apiPath = $"api/{appConfig.Token}"

  let webhookUrl =
    appConfig.Server.Address.ToString() + apiPath

  let validate (req: HttpListenerRequest) =
    req.Url.LocalPath = $"/{apiPath}"

  let webhook =
    { Listener = listener
      ValidateRequest = validate }

  let botConfig =
    { defaultConfig with
        Token = appConfig.Token
        WebHook = Some webhook }

  let printError e =
    printfn "Failed creating webhook:"
    printfn "%A" e

  let printStarted () =
    printfn
      "Bot started! Listening to %s"
      appConfig.Server.Listens

  let setWebhook () =
    setWebhookBase webhookUrl None None None
    |> api botConfig
    |> Async.map (Result.mapError printError)
    |> Async.Ignore

  let startBot () =
    printStarted ()
    startBot botConfig (Api.onUpdate getConnection appConfig.Storage.Path) None

  async {
    do! setWebhook ()
    do! startBot () }
  |> Async.Ignore

[<EntryPoint>]
let main _ =
  let printNoConnection () =
    printfn "%s"
      <| "Error connecting to database. "
      +  "Probably the problem is with connection details."

  let config = Config.load "Config.json"

  use listener = new HttpListener()
  listener.Prefixes.Add(config.Server.Listens)

  let getConnection () =
    Sql.host config.Database.Host
    |> Sql.database config.Database.Database
    |> Sql.username config.Database.Username
    |> Sql.password config.Database.Password
    |> Sql.port config.Database.Port
    |> Sql.formatConnectionString
    |> Sql.connect
    |> Sql.createConnection

  let connected =
    try
      // Test connection
      using (getConnection()) (fun c -> c.Open())
      true

    with
    | _ ->
      printNoConnection ()
      false

  if connected then
    // Run synchronously to block the tread
    // Don't use Async.StartImmediate or the
    // program will immediately shut after the launch
    startBot config listener getConnection
    |> Async.Ignore
    |> Async.RunSynchronously

  0 // Return an integer exit code
