module InsightClub.Creator.Bot.Program

open System.Net
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open FsToolkit.ErrorHandling
open Helpers
open Config
open Context
open Api


let startBot
  (appConfig: Config)
  (listener: HttpListener)
  (dbContext: Context) =
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

  let setWebHook () =
    setWebhookBase webhookUrl None None None
    |> api botConfig
    |> Async.map (Result.map ignore >> Result.mapError printError)

  let start () =
    printStarted ()
    startBot botConfig (updateArrived appConfig dbContext) None

  asyncResult
    { do! setWebHook ()
      do! start () }
  |> Async.Ignore

[<EntryPoint>]
let main _ =
  let filePath =
    #if DEBUG
      "../../../Config.yaml"
    #else
      "Config.yaml"
    #endif

  let printNoFile () =
    printfn $"Please, provide a {filePath} file."

  let printNoConnection () =
    printfn "%s"
      <| "Error connecting to database. "
      +  "Probably the problem is with connection details."

  // Run synchronously to block the tread
  // Don't use Async.StartImmediate
  // or program will immediately shut after the launch
  asyncOption
    { let! config =
        filePath
        |> Config.tryLoad
        |> Option.onNone printNoFile

      use listener = new HttpListener()
      listener.Prefixes.Add(config.Server.Listen)

      use context =
        config
        |> Config.connStr
        |> Context.create

      if Context.canConnect context
      then do! startBot config listener context
      else printNoConnection () }
  |> Async.Ignore
  |> Async.RunSynchronously

  0 // Return an integer exit code
