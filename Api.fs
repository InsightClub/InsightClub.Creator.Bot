module InsightClub.Creator.Bot.Api

open Core
open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open FsToolkit.ErrorHandling
open Microsoft.FSharpLu.Json


module Json = Compact.Strict

let (|NotCommand|_|) (s: string) =
  if s.StartsWith("/") then Some s else None

let always x _ = x

let getUser ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

let getInactive ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.Text)
  |> Option.filter ((=) "/start")
  |> Option.map (always Inactive.Start)
  |> always

let getIdle ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.Text)
  |> Option.filter ((=) "/new")
  |> Option.map (always Idle.CreateCourse)
  |> always

let getCreatingCourse ctx =
  always <|
  match ctx.Update with
  | { CallbackQuery = Some { Data = Some "/cancel" } } ->
    Some CreatingCourse.Cancel

  | { Message = Some { Text = Some (NotCommand courseTitle) } } ->
    Some <| CreatingCourse.CreateCourse courseTitle

  | _ ->
    None

let getEditingCourse ctx =
  ctx.Update.CallbackQuery
  |> Option.bind (fun q -> q.Data)
  |> Option.filter ((=) "/exit")
  |> Option.map (always EditingCourse.Exit)
  |> always

let getCommands ctx =
  { getInactive = getInactive ctx
    getIdle = getIdle ctx
    getCreatingCourse = getCreatingCourse ctx
    getEditingCourse = getEditingCourse ctx }

let updateArrived botConfig getConnection upContext =
  Async.singleton ()
  |> Async.Ignore
