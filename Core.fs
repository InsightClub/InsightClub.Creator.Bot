module InsightClub.Creator.Bot.Core


// Types
type CourseId = int
type CourseTitle = string

module Inactive =
  type Command = Start

module Idle =
  type Command = CreateCourse
  type Context = Started | Canceled | Exited | Error

module CreatingCourse =
  type Command = Cancel | CreateCourse of CourseTitle
  type Context = Started | TitleReserved | Error

module EditingCourse =
  type Command = Exit

type BotState =
  | Inactive
  | Idle of Idle.Context
  | CreatingCourse of CreatingCourse.Context
  | EditingCourse of CourseId

type Command<'c> = Recognized of 'c | Unknown
type CommandGetter<'c> = unit -> Command<'c>

type BotCommands =
  { getInactive: CommandGetter<Inactive.Command>
    getIdle: CommandGetter<Idle.Command>
    getCreatingCourse: CommandGetter<CreatingCourse.Command>
    getEditingCourse: CommandGetter<EditingCourse.Command> }

type Service<'p, 'a> = ('p -> 'a) -> 'a

type BotServices<'a> =
  { isCourseTitleReserved: CourseTitle -> Service<bool, 'a>
    createCourse: CourseTitle -> Service<CourseId, 'a> }

// Values
let initial = Inactive

let private updateInactive callback =
  function
  | Recognized Inactive.Start ->
    callback <| Idle Idle.Started

  | Unknown ->
    callback Inactive

let private updateIdle callback =
  function
  | Recognized Idle.CreateCourse ->
    callback <| CreatingCourse CreatingCourse.Started

  | Unknown ->
    callback <| Idle Idle.Error

let private updatePendingCourseTitle services callback =
  function
  | Recognized CreatingCourse.Cancel ->
    callback <| Idle Idle.Canceled

  | Recognized (CreatingCourse.CreateCourse title) ->
    ( function
      | true  ->
        callback <| CreatingCourse CreatingCourse.TitleReserved

      | false ->
        EditingCourse
        >> callback
        |> services.createCourse title )

    |> services.isCourseTitleReserved title

  | Unknown ->
    callback <| CreatingCourse CreatingCourse.Error

let private updateEditingCourse callback courseId =
  function
  | Recognized EditingCourse.Exit ->
    callback <| Idle Idle.Exited

  | Unknown ->
    callback <| EditingCourse courseId

let update commands services callback =
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive callback

  | Idle _ ->
    commands.getIdle ()
    |> updateIdle callback

  | CreatingCourse _ ->
    commands.getCreatingCourse ()
    |> updatePendingCourseTitle services callback

  | EditingCourse courseId ->
    commands.getEditingCourse ()
    |> updateEditingCourse callback courseId
