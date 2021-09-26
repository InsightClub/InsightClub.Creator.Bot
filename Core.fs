module InsightClub.Creator.Bot.Core


// Types
type CourseId = int
type CourseTitle = string

module Inactive =
  type Command = Start

module Idle =
  type Command = CreateCourse
  type Data = Started | CourseCanceled | ExitedEditing | Error

module CreatingCourse =
  type Command = Cancel | CreateCourse of CourseTitle
  type Data = Started | TitleReserved | Error

module EditingCourse =
  type Command = Exit

type BotState =
  | Inactive
  | Idle of Idle.Data
  | CreatingCourse of CreatingCourse.Data
  | EditingCourse of CourseId

type CommandGetter<'c> = unit -> Option<'c>

type BotCommands =
  { getInactive: CommandGetter<Inactive.Command>
    getIdle: CommandGetter<Idle.Command>
    getCreatingCourse: CommandGetter<CreatingCourse.Command>
    getEditingCourse: CommandGetter<EditingCourse.Command> }

type Service<'p, 'a> = ('p -> 'a) -> 'a

type BotServices<'a> =
  { tryCreateCourse: CourseTitle -> Service<CourseId option, 'a> }

// Values
let initial = Inactive

let private updateInactive callback =
  function
  | Some Inactive.Start ->
    callback <| Idle Idle.Started

  | None ->
    callback Inactive

let private updateIdle callback =
  function
  | Some Idle.CreateCourse ->
    callback <| CreatingCourse CreatingCourse.Started

  | None ->
    callback <| Idle Idle.Error

let private updateCreatingCourse services callback =
  function
  | Some CreatingCourse.Cancel ->
    callback <| Idle Idle.CourseCanceled

  | Some (CreatingCourse.CreateCourse title) ->
    ( function
      | Some courseId ->
        callback <| EditingCourse courseId

      | None ->
        callback <| CreatingCourse CreatingCourse.TitleReserved )
    |> services.tryCreateCourse title

  | None ->
    callback <| CreatingCourse CreatingCourse.Error

let private updateEditingCourse callback courseId =
  function
  | Some EditingCourse.Exit ->
    callback <| Idle Idle.ExitedEditing

  | None ->
    callback <| EditingCourse courseId

let update services commands callback =
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive callback

  | Idle _ ->
    commands.getIdle ()
    |> updateIdle callback

  | CreatingCourse _ ->
    commands.getCreatingCourse ()
    |> updateCreatingCourse services callback

  | EditingCourse courseId ->
    commands.getEditingCourse ()
    |> updateEditingCourse callback courseId
