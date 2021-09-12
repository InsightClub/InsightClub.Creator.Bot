module InsightClub.Creator.Bot.Program

open System.Net
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open FsToolkit.ErrorHandling
open InsightClub.Creator.Bot.Config
open InsightClub.Db


let startBot
  (appConfig: Config)
  (listener: HttpListener)
  (getContext: unit -> BotContext) =
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
    startBot botConfig (Api.updateArrived botConfig getContext) None

  asyncResult
    { do! setWebhook ()
      do! startBot () }
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

  let configOption = Config.tryLoad filePath

  if configOption.IsSome then
    let config = configOption.Value

    use listener = new HttpListener()
    listener.Prefixes.Add(config.Server.Listen)

    let getContext () =
      config
      |> Config.connStr
      |> BotContext.create

    if BotContext.canConnect <| getContext () then

      // Run synchronously to block the tread
      // Don't use Async.StartImmediate
      // or program will immediately shut after the launch
      startBot config listener getContext
      |> Async.Ignore
      |> Async.RunSynchronously

    else
      printNoConnection ()

  else
    printNoFile ()

  0 // Return an integer exit code
