module InsightClub.Creator.Bot.Core


// Types
type Text = string
type FilePath = string
type CourseName = string
type CourseDescription = string

[<RequireQualifiedAccess>]
type BotData =
  | Text of Text
  | Voice of FilePath

type CourseDataAccumulator =
  { Name: CourseName
    Description: CourseDescription
    Blocks: BotData list }

[<RequireQualifiedAccess>]
type BotState =
  | Inactive
  | Idle
  | NewCoursePendingName
  | NewCoursePendingDesc of CourseName
  | NewCourseAccumulator of CourseDataAccumulator

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
    BotState.NewCoursePendingName

  | _ ->
    BotState.Idle

let updateNewCoursePendingName =
  function
  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle

  | BotEvent.DataReceived (BotData.Text courseName) ->
    BotState.NewCoursePendingDesc courseName

  | _ ->
    BotState.NewCoursePendingName

let updateNewCoursePendingDesc courseName =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    BotState.NewCoursePendingName

  | BotEvent.CommandReceived BotCommand.Skip ->
    BotState.NewCourseAccumulator
      { Name = courseName
        Description = ""
        Blocks = [] }

  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle

  | BotEvent.DataReceived (BotData.Text courseDescription) ->
    BotState.NewCourseAccumulator
      { Name = courseName
        Description = courseDescription
        Blocks = [] }

  | _ ->
    BotState.NewCoursePendingDesc courseName

let updateNewCourseAccumulator acc =
  function
  | BotEvent.CommandReceived BotCommand.Undo ->
    match acc.Blocks with
    | [] ->
      BotState.NewCoursePendingDesc acc.Name

    | _ :: xs ->
      BotState.NewCourseAccumulator acc

  | BotEvent.CommandReceived BotCommand.Cancel ->
    BotState.Idle

  | BotEvent.CommandReceived BotCommand.Finish ->
    BotState.Idle

  | BotEvent.DataReceived (BotData.Text text) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Text text) :: acc.Blocks }

    BotState.NewCourseAccumulator newAcc

  | BotEvent.DataReceived (BotData.Voice filePath) ->
    let newAcc =
      { acc with
          Blocks = (BotData.Voice filePath) :: acc.Blocks }

    BotState.NewCourseAccumulator newAcc

  | _ ->
    BotState.NewCourseAccumulator acc

let updateState state event =
  match state with
  | BotState.Inactive ->
    updateInactive event

  | BotState.Idle ->
    updateIdle event

  | BotState.NewCoursePendingName ->
    updateNewCoursePendingName event

  | BotState.NewCoursePendingDesc courseName ->
    updateNewCoursePendingDesc courseName event

  | BotState.NewCourseAccumulator acc ->
    updateNewCourseAccumulator acc event
