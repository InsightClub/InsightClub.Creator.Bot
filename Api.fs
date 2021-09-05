module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open FsToolkit.ErrorHandling
open Core
open Model
open Repo


let commands =
  Map.ofList
    [ ("/start", BotCommand.Start)
      ("/help", BotCommand.Help)
      ("/new", BotCommand.New)
      ("/undo", BotCommand.Undo)
      ("/skip", BotCommand.Skip)
      ("/cancel", BotCommand.Cancel)
      ("/finish", BotCommand.Finish) ]

let getEvent ctx =
  match ctx.Update.Message with
  | Some { Text = Some text } ->
    let textData =
      BotData.Text text
      |> BotEvent.DataReceived

    commands
    |> Map.tryFind text
    |> Option.map BotEvent.CommandReceived
    |> Option.defaultValue textData

  | Some { Voice = Some voice } ->
    voice.FileId
    |> BotData.Voice
    |> BotEvent.DataReceived

  | _ ->
    BotEvent.UnsupportedReceived

let tryGetUser ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

let apiIgnore config req =
  req
  |> api config
  |> Async.Ignore

let sendMessageAsync config chatId message =
  sendMessage chatId message
  |> apiIgnore config

let handleIntent user entity newState intent config =
  ()

// Stub
let services =
  { checkNameReserved = fun _ -> Async.singleton false }

let updateArrived dbContext upContext =
  asyncOption
    { let! user = tryGetUser upContext
      let! creator = getCreatorAsync dbContext user.Id
      let event = getEvent upContext
      let! newState, intent = updateState services creator.BotState event
      let config = upContext.Config

      handleIntent user creator newState intent config }
  |> Async.Ignore
  |> Async.StartImmediate
