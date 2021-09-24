module InsightClub.Creator.Bot.Program

open Config
open FsToolkit.ErrorHandling
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
  // YamlConfig adds additional '/' character at the end of urls
  // So don't prepend apiPath with '/'
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
      appConfig.Server.Listen

  let setWebhook () =
    setWebhookBase webhookUrl None None None
    |> api botConfig
    |> Async.map (Result.map ignore >> Result.mapError printError)

  let startBot () =
    printStarted ()
    startBot botConfig (Api.updateArrived botConfig getConnection) None

  asyncResult
    { do! setWebhook ()
      do! startBot () }
  |> Async.Ignore

[<EntryPoint>]
let main _ =
  let filePath = "Config.yaml"

  let printNoFile () =
    printfn $"Please, provide a {filePath} file."

  let printNoConnection () =
    printfn "%s"
      <| "Error connecting to database. "
      +  "Probably the problem is with connection details."

  let configOption = Config.tryLoad filePath

  if configOption.IsSome then
    let config = configOption.Value

    use listener = new HttpListener()
    listener.Prefixes.Add(config.Server.Listen)

    let getConnection () =
      let str =
        Sql.host config.Db.Host
        |> Sql.database config.Db.Database
        |> Sql.username config.Db.Username
        |> Sql.password config.Db.Password
        |> Sql.port config.Db.Port
        |> Sql.formatConnectionString

      new NpgsqlConnection(str)

    try
      // Test connection
      using (getConnection()) (fun c -> c.Open())

      // Run synchronously to block the tread
      // Don't use Async.StartImmediate
      // or program will immediately shut after the launch
      startBot config listener getConnection
      |> Async.Ignore
      |> Async.RunSynchronously

    with
    | _ -> printNoConnection ()

  else
    printNoFile ()

  0 // Return an integer exit code
