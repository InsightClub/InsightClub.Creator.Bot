module InsightClub.Creator.Bot.Commands

open Core
open System
open Funogram.Telegram.Types


type QueryEffect =
  | ShowDesc of Core.CourseDesc
  | InformMin
  | InformMax

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
let add = "/add"
let back = "/back"

let private (|Command|_|) command = function
  | { Message.Text = Some text }
    when text = command -> Some ()
  | _                   -> None

let private (|Text|_|) = function
  | { Message.Text = Some text } -> Some text
  | _                            -> None

let private (|CommandQ|_|) command = function
| { CallbackQuery.Data = Some text }
  when text = command -> Some ()
| _                   -> None

let private (|ParamQ|_|) command = function
  | { CallbackQuery.Data = Some text }
    when text.StartsWith(command + " ") ->
    let start = String.length command + 1
    tryParseWith Int32.TryParse text.[ start .. ]

  | _ -> None

let onMessage message : BotCommands<unit> =
  let getInactive () =
    match message with
    | Command start -> Some Inactive.Start
    | _             -> None

  let getIdle () =
    match message with
    | Command help -> Some Idle.Help
    | Command new' -> Some Idle.CreateCourse
    | Command edit -> Some <| Idle.EditCourse 5
    | _            -> None

  let getCreatingCourse () =
    match message with
    | Text title -> Some <| CreatingCourse.CreateCourse title
    | _          -> None

  let getEditingCourse () = None

  let getEditingTitle () =
    match message with
    | Text title -> Some <| EditingTitle.SetTitle title
    | _          -> None

  let getEditingDesc () =
    match message with
    | Text desc -> Some <| EditingDesc.SetDesc desc
    | _         -> None

  let getListingCourses () = None

  let getCreatingBlock () =
    match message with
    | Text title -> Some <| CreatingBlock.CreateBlock title
    | _          -> None

  let getEditingBlock () = None

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse
    getEditingTitle = getEditingTitle
    getEditingDesc = getEditingDesc
    getListingCourses = getListingCourses
    getCreatingBlock = getCreatingBlock
    getEditingBlock = getEditingBlock }

let onQuery query =
  let getInactive () = None

  let getIdle () = None

  let getCreatingCourse () =
    match query with
    | CommandQ cancel -> Some CreatingCourse.Cancel
    | _               -> None

  let getEditingCourse () =
    match query with
    | CommandQ title -> Some EditingCourse.EditTitle
    | CommandQ desc  -> Some EditingCourse.EditDesc
    | CommandQ exit  -> Some EditingCourse.Exit
    | CommandQ add   -> Some EditingCourse.AddBlock
    | _              -> None

  let getEditingTitle () =
    match query with
    | CommandQ cancel -> Some EditingTitle.Cancel
    | _               -> None

  let getEditingDesc () =
    match query with
    | CommandQ show   -> Some <| EditingDesc.Show QueryEffect.ShowDesc
    | CommandQ cancel -> Some EditingDesc.Cancel
    | _               -> None

  let getListingCourses () =
    match query with
    | ParamQ edit id -> Some <| ListingCourses.Select id
    | CommandQ prev  -> Some <| ListingCourses.Prev QueryEffect.InformMin
    | CommandQ next  -> Some <| ListingCourses.Next QueryEffect.InformMax
    | CommandQ exit  -> Some ListingCourses.Exit
    | _              -> None

  let getCreatingBlock () =
    match query with
    | CommandQ cancel -> Some CreatingBlock.Cancel
    | _               -> None

  let getEditingBlock () =
    match query with
    | CommandQ back -> Some EditingBlock.Back
    | CommandQ next -> Some EditingBlock.CreateNext
    | _             -> None

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse
    getEditingTitle = getEditingTitle
    getEditingDesc = getEditingDesc
    getListingCourses = getListingCourses
    getCreatingBlock = getCreatingBlock
    getEditingBlock = getEditingBlock }
