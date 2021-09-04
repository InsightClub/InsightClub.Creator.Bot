module InsightClub.Creator.Bot.Core


// Types
type Text = string
type FilePath = string
type CourseName = string
type CourseDesc = string

[<RequireQualifiedAccess>]
type BotData =
  | Text of Text
  | Voice of FilePath

type BotDataAccumulator =
  { Name: CourseName
    Desc: CourseDesc
    Blocks: BotData list }

[<RequireQualifiedAccess>]
type BotState =
  | Inactive
  | Idle
  | PendingName
  | PendingDesc of CourseName
  | PendingData of BotDataAccumulator

[<RequireQualifiedAccess>]
type BotCommand =
  | Start
  | Help
  | New
  | Undo
  | Skip
  | Cancel
  | Finish

[<RequireQualifiedAccess>]
type BotEvent =
  | CommandReceived of BotCommand
  | DataReceived of BotData
  | UnknownReceived

// Values
let initialState = BotState.Inactive

let updateInactive =
  function
  | BotEvent.CommandReceived BotCommand.Start ->
    BotState.Idle
  | _ ->
    BotState.Inactive

let updateIdle =
  function
  | BotEvent.CommandReceived BotCommand.New ->
    BotState.PendingName

  | _ ->
    BotState.Idle

let updatePendingName =
  function
  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle

  | BotEvent.DataReceived (BotData.Text courseName) ->
    BotState.PendingDesc courseName

  | _ ->
    BotState.PendingName

let updatePendingDesc courseName =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    BotState.PendingName

  | BotEvent.CommandReceived BotCommand.Skip ->
    BotState.PendingData
      { Name = courseName
        Desc = ""
        Blocks = [] }

  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle

  | BotEvent.DataReceived (BotData.Text courseDesc) ->
    BotState.PendingData
      { Name = courseName
        Desc = courseDesc
        Blocks = [] }

  | _ ->
    BotState.PendingDesc courseName

let updatePendingData acc =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    match acc.Blocks with
    | [] ->
      BotState.PendingDesc acc.Name

    | _ :: xs ->
      BotState.PendingData acc

  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle

  | BotEvent.CommandReceived BotCommand.Finish ->
    BotState.Idle

  | BotEvent.DataReceived (BotData.Text text) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Text text) :: acc.Blocks }

    BotState.PendingData newAcc

  | BotEvent.DataReceived (BotData.Voice filePath) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Voice filePath) :: acc.Blocks }

    BotState.PendingData newAcc

  | _ ->
    BotState.PendingData acc

let updateState state event =
  match state with
  | BotState.Inactive ->
    updateInactive event

  | BotState.Idle ->
    updateIdle event

  | BotState.PendingName ->
    updatePendingName event

  | BotState.PendingDesc courseName ->
    updatePendingDesc courseName event

  | BotState.PendingData acc ->
    updatePendingData acc event
