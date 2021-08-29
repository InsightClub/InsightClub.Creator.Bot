module InsightClub.Creator.Bot.Program

open System.Net
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Api


let bot =
  // YamlConfig adds additional '/' character at the end of urls
  // So don't prepend apiPath with '/'
  let apiPath = $"api/{Config.Token}"

  let webSocketEndpoint = Config.Server.Address.ToString() + apiPath

  let botConfig =
    { defaultConfig with
        Token = Config.Token }

  async
    { let! hook =
        setWebhookBase webSocketEndpoint None None None
        |> api botConfig

      match hook with
      | Ok _ ->
        printfn "---------------Starting server---------------"
        printfn "Server will listen at %s" Config.Server.ListenTo

        use listener = new HttpListener()

        listener
          .Prefixes
          .Add(Config.Server.ListenTo)

        let validate (req: HttpListenerRequest) =
          req.Url.LocalPath = $"/{apiPath}"

        let webhook =
          { Listener = listener
            ValidateRequest = validate }

        let botConfig =
          { botConfig with
              WebHook = Some webhook }

        do! startBot botConfig updateArrived None

      | Error e ->
        printfn "---------------Can't set webhook---------------"
        printfn "%A" e }

[<EntryPoint>]
let main _ =
  // Run synchronously to block the tread
  // Don't use Async.StartImmediate
  // or program will immediately shut after the launch
  Async.RunSynchronously bot

  0 // Return an integer exit code
