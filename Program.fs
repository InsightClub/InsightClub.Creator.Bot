module Insight.Creator.Bot.Program

open System.Net
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Api


let botConfig =
  { defaultConfig with
      Token = Config.Token }

let bot =
  async {
    // YamlConfig adds additional / character at the end of urls
    // So don't prepend apiPath with /
    let apiPath = $"api/{Config.Token}"
    let webSocketEndpoint = Config.Server.Address.ToString() + apiPath

    let! hook =
      setWebhookBase webSocketEndpoint None None None
      |> api botConfig

    match hook with
    | Ok _ ->
      use listener = new HttpListener()
      listener.Prefixes.Add(Config.Server.ListenTo)
      listener.Start()

      let webhook =
        { Listener = listener
          ValidateRequest = (fun req -> req.Url.LocalPath = $"/{apiPath}") }

      printfn "---------------Starting server---------------"
      printfn "Server will listen at %s" Config.Server.ListenTo

      return!
        startBot
          { botConfig with
              WebHook = Some webhook }
          updateArrived
          None

    | Error e ->
      printfn "---------------Can't set webhook---------------"
      printfn "%A" e
      return ()
  }

[<EntryPoint>]
let main _ =
  Async.RunSynchronously bot
  0 // return an integer exit code
