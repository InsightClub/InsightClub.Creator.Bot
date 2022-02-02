module InsightClub.Creator.Bot.Dispatcher

open Core
open System
open Funogram.Telegram


type Message = Types.Message
type CallbackQuery = Types.CallbackQuery
type PhotoSize = Types.PhotoSize

type QueryEffect =
  | ShowContent of Content list
  | InformMin
  | InformMax
  | InformEmpty

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
let before = "/before"
let after = "/after"
let nothing = "/nothing"
let clean = "/clean"

let private getBiggest =
  Seq.maxBy (fun (s: PhotoSize) -> s.Width)

let private (|Command|_|) command = function
  | { Message.Text = Some text }
    when text = command -> Some ()
  | _                   -> None

let private (|Text|_|) = function
  | { Message.Text = Some text } -> Some text
  | _                            -> None

let private (|Photo|_|) = function
| { Message.Photo = Some sizes } -> Some <| (getBiggest sizes).FileId
| _                              -> None

let private (|Audio|_|) = function
| { Message.Audio = Some audio } -> Some audio.FileId
| _                              -> None

let private (|Video|_|) = function
| { Message.Video = Some video } -> Some video.FileId
| _                              -> None

let private (|Voice|_|) = function
| { Message.Voice = Some voice } -> Some voice.FileId
| _                              -> None

let private (|Document|_|) = function
| { Message.Document = Some document } -> Some document.FileId
| _                                    -> None

let private (|VideoNote|_|) = function
| { Message.VideoNote = Some note } -> Some note.FileId
| _                                 -> None

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

let dispatchMessage message : BotDispatcher<unit> =
  let askInactive () =
    match message with
    | Command start -> Some Inactive.Start
    | _             -> None

  let askIdle () =
    match message with
    | Command help -> Some Idle.Help
    | Command new' -> Some Idle.CreateCourse
    | Command edit -> Some <| Idle.EditCourse 5
    | _            -> None

  let askCreatingCourse () =
    match message with
    | Text title -> Some <| CreatingCourse.CreateCourse title
    | _          -> None

  let askEditingCourse () = None

  let askEditingTitle () =
    match message with
    | Text title -> Some <| EditingTitle.SetTitle title
    | _          -> None

  let askEditingDesc () =
    match message with
    | Text desc -> Some <| EditingDesc.SetDesc desc
    | _         -> None

  let askListingCourses () = None

  let askCreatingBlock () =
    match message with
    | Text title -> Some <| CreatingBlock.CreateBlock title
    | _          -> None

  let askEditingBlock () =
    match message with
    | Text text        -> Some <| EditingBlock.AddContent (Text text)
    | Photo fileId     -> Some <| EditingBlock.AddContent (Photo fileId)
    | Audio fileId     -> Some <| EditingBlock.AddContent (Audio fileId)
    | Video fileId     -> Some <| EditingBlock.AddContent (Video fileId)
    | Voice fileId     -> Some <| EditingBlock.AddContent (Voice fileId)
    | Document fileId  -> Some <| EditingBlock.AddContent (Document fileId)
    | VideoNote fileId -> Some <| EditingBlock.AddContent (VideoNote fileId)
    | _                -> None

  let askListingBlocks () = None

  { askInactive = askInactive
    askIdle = askIdle
    askCreatingCourse = askCreatingCourse
    askEditingCourse = askEditingCourse
    askEditingTitle = askEditingTitle
    askEditingDesc = askEditingDesc
    askListingCourses = askListingCourses
    askCreatingBlock = askCreatingBlock
    askEditingBlock = askEditingBlock
    askListingBlocks = askListingBlocks }

let dispatchCallbackQuery query =
  let askInactive () = None

  let askIdle () = None

  let askCreatingCourse () =
    match query with
    | CommandQ cancel -> Some CreatingCourse.Cancel
    | _               -> None

  let askEditingCourse () =
    match query with
    | CommandQ title -> Some EditingCourse.EditTitle
    | CommandQ desc  -> Some EditingCourse.EditDesc
    | CommandQ exit  -> Some EditingCourse.Exit
    | CommandQ add   -> Some EditingCourse.AddBlock
    | CommandQ edit  -> Some <| EditingCourse.EditBlock 5
    | _              -> None

  let askEditingTitle () =
    match query with
    | CommandQ cancel -> Some EditingTitle.Cancel
    | _               -> None

  let askEditingDesc () =
    match query with
    | CommandQ cancel -> Some EditingDesc.Cancel
    | _               -> None

  let askListingCourses () =
    match query with
    | ParamQ edit id -> Some <| ListingCourses.Select id
    | CommandQ prev  -> Some <| ListingCourses.Prev QueryEffect.InformMin
    | CommandQ next  -> Some <| ListingCourses.Next QueryEffect.InformMax
    | CommandQ exit  -> Some ListingCourses.Exit
    | _              -> None

  let askCreatingBlock () =
    match query with
    | CommandQ cancel -> Some CreatingBlock.Cancel
    | _               -> None

  let askEditingBlock () =
    match query with
    | CommandQ back    -> Some EditingBlock.Back
    | CommandQ nothing -> Some EditingBlock.Nothing
    | CommandQ before  -> Some EditingBlock.InsertBefore
    | CommandQ after   -> Some EditingBlock.InsertAfter
    | CommandQ prev    -> Some <| EditingBlock.Prev QueryEffect.InformMin
    | CommandQ next    -> Some <| EditingBlock.Next QueryEffect.InformMax
    | CommandQ clean   -> Some <| EditingBlock.Clean QueryEffect.InformEmpty
    | CommandQ show    -> Some <| EditingBlock.Show QueryEffect.ShowContent
    | _                -> None

  let askListingBlocks () =
    match query with
    | ParamQ edit id -> Some <| ListingBlocks.Select id
    | CommandQ prev  -> Some <| ListingBlocks.Prev QueryEffect.InformMin
    | CommandQ next  -> Some <| ListingBlocks.Next QueryEffect.InformMax
    | CommandQ back  -> Some ListingBlocks.Back
    | _              -> None

  { askInactive = askInactive
    askIdle = askIdle
    askCreatingCourse = askCreatingCourse
    askEditingCourse = askEditingCourse
    askEditingTitle = askEditingTitle
    askEditingDesc = askEditingDesc
    askListingCourses = askListingCourses
    askCreatingBlock = askCreatingBlock
    askEditingBlock = askEditingBlock
    askListingBlocks = askListingBlocks }
