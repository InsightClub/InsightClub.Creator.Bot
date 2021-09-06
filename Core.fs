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

type CheckNameReserved<'a> = Text -> (bool -> 'a) -> 'a

type BotServices<'a> =
  { checkNameReserved: CheckNameReserved<'a> }

// Values
let initialState = BotState.Inactive

let updateInactive callback =
  function
  | BotEvent.CommandReceived BotCommand.Start ->
    callback BotState.Idle BotIntent.ReportStarted
  | _ ->
    callback BotState.Inactive BotIntent.Ignore

let updateIdle callback =
  function
  | BotEvent.CommandReceived BotCommand.New ->
    callback BotState.PendingName BotIntent.ReportStarted

  | BotEvent.CommandReceived BotCommand.Help ->
    callback BotState.Idle BotIntent.ShowHelp

  | _ ->
    callback BotState.Idle BotIntent.ReportUnsupported

let updatePendingName callback checkNameReserved =
  function
  | BotEvent.CommandReceived BotCommand.Cancel ->
    callback BotState.Idle BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text courseName) ->
    let answer isReserved =
      if isReserved
      then callback BotState.PendingName BotIntent.ReportNameReserved
      else callback (BotState.PendingDesc courseName) BotIntent.ReportNameSet

    checkNameReserved courseName answer

  | BotEvent.CommandReceived BotCommand.Help ->
    callback BotState.PendingName BotIntent.ShowHelp

  | _ ->
    callback BotState.PendingName BotIntent.ReportUnsupported

let updatePendingDesc callback courseName =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    callback BotState.PendingName BotIntent.ReportNameUndone

  | BotEvent.CommandReceived BotCommand.Skip ->
    let nextState =
      BotState.PendingData
        { Name = courseName
          Desc = ""
          Blocks = [] }

    callback nextState BotIntent.ReportDescSkipped

  | BotEvent.CommandReceived BotCommand.Cancel ->
    callback BotState.Idle BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text courseDesc) ->
    let nextState =
      BotState.PendingData
        { Name = courseName
          Desc = courseDesc
          Blocks = [] }
    callback nextState BotIntent.ReportDescSet

  | BotEvent.CommandReceived BotCommand.Help ->
    callback (BotState.PendingDesc courseName) BotIntent.ShowHelp

  | _ ->
    callback (BotState.PendingDesc courseName) BotIntent.ReportUnsupported

let updatePendingData callback acc =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    match acc.Blocks with
    | [] ->
      callback (BotState.PendingDesc acc.Name) BotIntent.ReportDescUndone

    | _ :: xs ->
      callback (BotState.PendingData acc) BotIntent.ReportDataUndone

  | BotEvent.CommandReceived BotCommand.Cancel ->
    callback BotState.Idle BotIntent.ReportCourseCanceled

  | BotEvent.DataReceived (BotData.Text text) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Text text) :: acc.Blocks }

    callback (BotState.PendingData newAcc) BotIntent.ReportDataTextSet

  | BotEvent.DataReceived (BotData.Voice filePath) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Voice filePath) :: acc.Blocks }

    callback (BotState.PendingData newAcc) BotIntent.ReportDataVoiceSet

  | BotEvent.CommandReceived BotCommand.Finish ->
    callback BotState.Idle BotIntent.ReportCourseFinished

  | BotEvent.CommandReceived BotCommand.Help ->
    callback (BotState.PendingData acc) BotIntent.ShowHelp

  | _ ->
    callback (BotState.PendingData acc) BotIntent.ReportUnsupported

let updateState callback services state event =
  match state with
  | BotState.Inactive ->
    updateInactive callback event

  | BotState.Idle ->
    updateIdle callback event

  | BotState.PendingName ->
    updatePendingName callback services.checkNameReserved event

  | BotState.PendingDesc courseName ->
    updatePendingDesc callback courseName event

  | BotState.PendingData acc ->
    updatePendingData callback acc event
