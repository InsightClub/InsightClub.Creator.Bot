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


let startBot (appConfig: Config) (dbContext: Context) =
  // YamlConfig adds additional '/' character at the end of urls
  // So don't prepend apiPath with '/'
  let apiPath = $"api/{appConfig.Token}"

  let webhookUrl =
    appConfig.Server.Address.ToString() + apiPath

  use listener = new HttpListener()
  listener.Prefixes.Add(appConfig.Server.Listen)

  let validate (req: HttpListenerRequest) =
    req.Url.LocalPath = $"/{apiPath}"

  let webhook =
    { Listener = listener
      ValidateRequest = validate }

  let botConfig =
    { defaultConfig with
        Token = appConfig.Token
        WebHook = Some webhook }

  let start _ =
    printfn "Bot started! Listening to %s" appConfig.Server.Listen
    startBot botConfig (updateArrived appConfig dbContext) None
    |> Async.RunSynchronously

  let printError e =
    printfn "Failed creating webhook:"
    printfn "%A" e

  // Run synchronously to block the tread
  // Don't use Async.StartImmediate
  // or program will immediately shut after the launch
  setWebhookBase webhookUrl None None None
  |> api botConfig
  |> Async.RunSynchronously
  |> Result.map start
  |> Result.mapError printError
  |> ignore

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

  let printNoContext () =
    printfn "%s"
      <| "Error connecting to database. "
      +  "Probably the problem is with connection details."

  let testConnection (context: Context) =
    if context.Database.CanConnect() then
      Some context
    else
      None

  option
    { let! config =
        Config.tryLoad filePath
        |> Option.onNone printNoFile

      let connStr =
        Config.connStr config

      use! context =
        Context.create connStr
        |> Option.tryCatch testConnection
        |> Option.flatten
        |> Option.onNone printNoContext

      startBot config context }
  |> ignore

  0 // Return an integer exit code
