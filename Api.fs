module InsightClub.Creator.Bot.Api

open Core
open Repo
open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open FsToolkit.ErrorHandling
open Microsoft.FSharpLu.Json


// Types
type TelegramBotState =
  { LastMessageId: int64 option
    State: BotState }

// Helpers
module Json = Compact.Strict

let (|NotCommand|_|) (s: string) =
  if s.StartsWith("/") then Some s else None

let always x _ = x

// Services
let getServices connection creatorId =
  let tryCreateCourse courseTitle callback =
    async
      { let! courseIdOption =
          tryCreateCourse connection creatorId courseTitle

        return! callback courseIdOption }

  { tryCreateCourse = tryCreateCourse }

// Commands
let getCommands ctx =
  let getInactive () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) "/start")
    |> Option.map (always Inactive.Start)

  let getIdle () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) "/new")
    |> Option.map (always Idle.CreateCourse)

  let getCreatingCourse () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some "/cancel" } } ->
      Some CreatingCourse.Cancel

    | { Message = Some { Text = Some (NotCommand courseTitle) } } ->
      Some <| CreatingCourse.CreateCourse courseTitle

    | _ ->
      None

  let getEditingCourse () =
    ctx.Update.CallbackQuery
    |> Option.bind (fun q -> q.Data)
    |> Option.filter ((=) "/exit")
    |> Option.map (always EditingCourse.Exit)

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse }

// State
let getState connection telegramId =
  getState (Json.serialize initial) connection telegramId
  |> Async.map Json.deserialize<TelegramBotState>

// Telegram user
let getUser ctx () =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

// Main function
let updateArrived getConnection upContext =
  Async.singleton ()
  |> Async.Ignore
