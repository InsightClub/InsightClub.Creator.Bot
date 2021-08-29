module InsightClub.Creator.Bot.Program

open System.IO
open System.Net
open FSharp.Configuration
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Api


type Config = YamlConfig<"Default.Config.yaml">

let startBot (appConfig: Config) =
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
    startBot botConfig updateArrived None
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
  let configFilePath =
  #if DEBUG
    "../../../Config.yaml"
  #else
    "Config.yaml"
  #endif

  let appConfig = Config()

  if (File.Exists configFilePath) then
    appConfig.Load(configFilePath)

    startBot appConfig
  else
    printfn "Please, provide a Config.yaml file"

  0 // Return an integer exit code
