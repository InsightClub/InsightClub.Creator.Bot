module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Core


// Event
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

// User
let tryGetUser ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

// Stub
let updateArrived ctx =
  ()
