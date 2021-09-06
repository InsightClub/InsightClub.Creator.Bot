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

type CheckNameReserved = string -> bool

type BotServices =
  { checkNameReserved: CheckNameReserved }

// Values
let initialState = BotState.Inactive

let updateInactive =
  function
  | BotEvent.CommandReceived BotCommand.Start ->
    BotState.Idle, BotIntent.ReportStarted
  | _ ->
    BotState.Inactive, BotIntent.Ignore

let updateIdle =
  function
  | BotEvent.CommandReceived BotCommand.New ->
    BotState.PendingName, BotIntent.ReportStarted

  | BotEvent.CommandReceived BotCommand.Help ->
    BotState.Idle, BotIntent.ShowHelp

  | _ ->
    BotState.Idle, BotIntent.ReportUnsupported

let updatePendingName checkNameReserved =
  function
  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle, BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text courseName) ->
    let reserved = checkNameReserved courseName

    if reserved
    then BotState.PendingName, BotIntent.ReportNameReserved
    else BotState.PendingDesc courseName, BotIntent.ReportNameSet

  | BotEvent.CommandReceived BotCommand.Help ->
    BotState.PendingName, BotIntent.ShowHelp

  | _ ->
    BotState.PendingName, BotIntent.ReportUnsupported

let updatePendingDesc courseName =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    BotState.PendingName, BotIntent.ReportNameUndone

  | BotEvent.CommandReceived BotCommand.Skip ->
    BotState.PendingData
      { Name = courseName
        Desc = ""
        Blocks = [] }
    , BotIntent.ReportDescSkipped

  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle, BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text courseDesc) ->
    BotState.PendingData
      { Name = courseName
        Desc = courseDesc
        Blocks = [] }
    , BotIntent.ReportDescSet

  | BotEvent.CommandReceived BotCommand.Help ->
    BotState.PendingDesc courseName, BotIntent.ShowHelp

  | _ ->
    BotState.PendingDesc courseName, BotIntent.ReportUnsupported

let updatePendingData acc =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    match acc.Blocks with
    | [] ->
      BotState.PendingDesc acc.Name, BotIntent.ReportDescUndone

    | _ :: xs ->
      BotState.PendingData acc, BotIntent.ReportDataUndone

  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle, BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text text) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Text text) :: acc.Blocks }

    BotState.PendingData newAcc, BotIntent.ReportDataTextSet

  | BotEvent.DataReceived (BotData.Voice filePath) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Voice filePath) :: acc.Blocks }

    BotState.PendingData newAcc, BotIntent.ReportDataVoiceSet

  | BotEvent.CommandReceived BotCommand.Finish ->
    BotState.Idle, BotIntent.ReportCourseFinished

  | BotEvent.CommandReceived BotCommand.Help ->
    BotState.PendingData acc, BotIntent.ShowHelp

  | _ ->
    BotState.PendingData acc, BotIntent.ReportUnsupported

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
