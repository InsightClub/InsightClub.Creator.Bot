module InsightClub.Creator.Bot.Core

open Helpers


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
  | UnsupportedReceived

[<RequireQualifiedAccess>]
type BotIntent =
  | Ignore
  | ShowHelp
  | ReportUnsupported
  | ReportStarted
  | ReportCourseStarted
  | ReportCourseCanceled
  | ReportNameReserved
  | ReportNameSet
  | ReportNameUndone
  | ReportDescSkipped
  | ReportDescSet
  | ReportDescUndone
  | ReportDataUndone
  | ReportDataTextSet
  | ReportDataVoiceSet
  | ReportCourseFinished

type CheckNameReserved = string -> Async<bool>

type BotServices =
  { checkNameReserved: CheckNameReserved }

// Values
let initialState = BotState.Inactive

let updateInactive =
  function
  | BotEvent.CommandReceived BotCommand.Start ->
    Async.pair BotState.Idle BotIntent.ReportStarted
  | _ ->
    Async.pair BotState.Inactive BotIntent.Ignore

let updateIdle =
  function
  | BotEvent.CommandReceived BotCommand.New ->
    Async.pair BotState.PendingName BotIntent.ReportStarted

  | BotEvent.CommandReceived BotCommand.Help ->
    Async.pair BotState.Idle BotIntent.ShowHelp

  | _ ->
    Async.pair BotState.Idle BotIntent.ReportUnsupported

let updatePendingName checkNameReserved =
  function
  | BotEvent.CommandReceived BotCommand.Cancel ->
    Async.pair BotState.Idle BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text courseName) ->
    async
      { let! reserved = checkNameReserved courseName
        if reserved then
          return
            BotState.PendingName,
            BotIntent.ReportNameReserved
        else
          return
            BotState.PendingDesc courseName,
            BotIntent.ReportNameSet }

  | BotEvent.CommandReceived BotCommand.Help ->
    Async.pair BotState.PendingName BotIntent.ShowHelp

  | _ ->
    Async.pair BotState.PendingName BotIntent.ReportUnsupported

let updatePendingDesc courseName =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    Async.pair BotState.PendingName BotIntent.ReportNameUndone

  | BotEvent.CommandReceived BotCommand.Skip ->
    Async.pair
      ( BotState.PendingData
          { Name = courseName
            Desc = ""
            Blocks = [] } )
      BotIntent.ReportDescSkipped

  | BotEvent.CommandReceived BotCommand.Cancel ->
    Async.pair BotState.Idle BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text courseDesc) ->
    Async.pair
      ( BotState.PendingData
          { Name = courseName
            Desc = courseDesc
            Blocks = [] } )
      BotIntent.ReportDescSet

  | BotEvent.CommandReceived BotCommand.Help ->
    Async.pair (BotState.PendingDesc courseName) BotIntent.ShowHelp

  | _ ->
    Async.pair (BotState.PendingDesc courseName) BotIntent.ReportUnsupported

let updatePendingData acc =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    match acc.Blocks with
    | [] ->
      Async.pair (BotState.PendingDesc acc.Name) BotIntent.ReportDescUndone

    | _ :: xs ->
      Async.pair (BotState.PendingData acc) BotIntent.ReportDataUndone

  | BotEvent.CommandReceived BotCommand.Cancel ->
    Async.pair BotState.Idle BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text text) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Text text) :: acc.Blocks }

    Async.pair (BotState.PendingData newAcc) BotIntent.ReportDataTextSet

  | BotEvent.DataReceived (BotData.Voice filePath) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Voice filePath) :: acc.Blocks }

    Async.pair (BotState.PendingData newAcc) BotIntent.ReportDataVoiceSet

  | BotEvent.CommandReceived BotCommand.Finish ->
    Async.pair BotState.Idle BotIntent.ReportCourseFinished

  | BotEvent.CommandReceived BotCommand.Help ->
    Async.pair (BotState.PendingData acc) BotIntent.ShowHelp

  | _ ->
    Async.pair (BotState.PendingData acc) BotIntent.ReportUnsupported

let updateState services state event =
  match state with
  | BotState.Inactive ->
    updateInactive event

  | BotState.Idle ->
    updateIdle event

  | BotState.PendingName ->
    updatePendingName services.checkNameReserved event

  | BotState.PendingDesc courseName ->
    updatePendingDesc courseName event

  | BotState.PendingData acc ->
    updatePendingData acc event
