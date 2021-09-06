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

let updateInactive callback event =
  ( match event with
    | BotEvent.CommandReceived BotCommand.Start ->
      BotState.Idle, BotIntent.ReportStarted
    | _ ->
      BotState.Inactive, BotIntent.Ignore )
  |> callback

let updateIdle callback event =
  ( match event with
    | BotEvent.CommandReceived BotCommand.New ->
      BotState.PendingName, BotIntent.ReportStarted

    | BotEvent.CommandReceived BotCommand.Help ->
      BotState.Idle, BotIntent.ShowHelp

    | _ ->
      BotState.Idle, BotIntent.ReportUnsupported )
  |> callback

let updatePendingName callback checkNameReserved event =
  match event with
  | BotEvent.CommandReceived BotCommand.Cancel ->
    callback (BotState.Idle, BotIntent.ReportCourseCanceled)

  | BotEvent.DataReceived (BotData.Text courseName) ->
    let answer isReserved =
      if isReserved
      then callback (BotState.PendingName, BotIntent.ReportNameReserved)
      else callback (BotState.PendingDesc courseName, BotIntent.ReportNameSet)

    checkNameReserved courseName answer

  | BotEvent.CommandReceived BotCommand.Help ->
    callback (BotState.PendingName, BotIntent.ShowHelp)

  | _ ->
    callback (BotState.PendingName, BotIntent.ReportUnsupported)

let updatePendingDesc callback courseName event =
  ( match event with
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
      BotState.PendingDesc courseName, BotIntent.ReportUnsupported )
  |> callback

let updatePendingData callback acc event =
  ( match event with
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
      BotState.PendingData acc, BotIntent.ReportUnsupported )
  |> callback

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
