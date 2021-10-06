module InsightClub.Creator.Bot.Command

open Core
open Funogram.Telegram.Bot
open Funogram.Telegram.Types


let start = "/start"
let help = "/help"
let new' = "/new"
let cancel = "/cancel"
let exit = "/exit"
let title = "/title"
let desc = "/desc"
let show = "/show"
let edit = "/edit"
let prev = "/prev"
let next = "/next"

let get ctx =
  let getInactive () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) start)
    |> Option.map (always Inactive.Start)

  let getIdle () =
    match ctx.Update.Message with
    | Some { Text = Some (Command help) } ->
      Some Idle.Help

    | Some { Text = Some (Command new') } ->
      Some Idle.CreateCourse

    | Some { Text = Some (Command edit) } ->
      Some (Idle.EditCourse 5)

    | _ ->
      None

  let getCreatingCourse () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command cancel) } } ->
      Some CreatingCourse.Cancel

    | { Message = Some { Text = Some (PlainText courseTitle) } } ->
      Some <| CreatingCourse.CreateCourse courseTitle

    | _ ->
      None

  let getEditingCourse () =
    match ctx.Update.CallbackQuery with
    | Some { Data = Some (Command title) } ->
      Some EditingCourse.EditTitle

    | Some { Data = Some (Command desc) } ->
      Some EditingCourse.EditDesc

    | Some { Data = Some (Command exit) } ->
      Some EditingCourse.Exit

    | _ ->
      None

  let getEditingTitle () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command cancel) } } ->
      Some EditingTitle.Cancel

    | { Message = Some { Text = Some (PlainText courseTitle) } } ->
      Some <| EditingTitle.SetTitle courseTitle

    | _ ->
      None

  let getEditingDesc () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command show) } } ->
      Some EditingDesc.Show

    | { CallbackQuery = Some { Data = Some (Command cancel) } } ->
      Some EditingDesc.Cancel

    | { Message = Some { Text = Some (PlainText courseDesc) } } ->
      Some <| EditingDesc.SetDesc courseDesc

    | _ ->
      None

  let getListingCourses () =
    match ctx.Update.CallbackQuery with
    | Some { Data = Some (CommandParam edit courseId) } ->
      Some (ListingCourses.Select courseId)

    | Some { Data = Some (Command prev) } ->
      Some ListingCourses.Prev

    | Some { Data = Some (Command next) } ->
      Some ListingCourses.Next

    | Some { Data = Some (Command exit) } ->
      Some ListingCourses.Exit

    | _ ->
      None

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse
    getEditingTitle = getEditingTitle
    getEditingDesc = getEditingDesc
    getListingCourses = getListingCourses }
