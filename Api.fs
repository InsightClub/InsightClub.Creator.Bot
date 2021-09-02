module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open FsToolkit.ErrorHandling
open Core
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

let tryGetEvent ctx =
  match ctx.Update.Message with
  | Some { Text = Some text } ->
    let textData =
      BotData.Text text
      |> BotEvent.DataReceived

    commands
    |> Map.tryFind text
    |> Option.map BotEvent.CommandReceived
    |> Option.defaultValue textData
    |> Some

  | Some { Voice = Some voice } ->
    voice.FileId
    |> BotData.Voice
    |> BotEvent.DataReceived
    |> Some

  | _ ->
    None

let tryGetUser ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

let handleUpdate telegramUser creator event newState =
  ()

let updateArrived config dbContext upContext =
  asyncOption
    { let! user = tryGetUser upContext
      let! creator = getCreatorAsync dbContext user.Id
      let! event = tryGetEvent upContext
      let newState = updateState creator.BotState event

      handleUpdate user creator event newState }
  |> Async.Ignore
  |> Async.StartImmediate
