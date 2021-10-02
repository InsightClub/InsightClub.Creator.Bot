module InsightClub.Creator.Bot.Core


// Types
type CourseId = int
type CourseTitle = string

module Inactive =
  type Command = Start

module Idle =
  type Command = Help | CreateCourse
  type Data = Started | Helping | CourseCanceled | ExitedEditing | Error

module CreatingCourse =
  type Command = Cancel | CreateCourse of CourseTitle
  type Data = Started | TitleReserved | Error

module EditingCourse =
  type Command = EditTitle | Exit
  type Data = CourseCreated | TitleCanceled | TitleSet | Error

module EditingTitle =
  type Command = Cancel | SetTitle of CourseTitle
  type Data = Started | TitleReserved | Error

type BotState =
  | Inactive
  | Idle of Idle.Data
  | CreatingCourse of CreatingCourse.Data
  | EditingCourse of CourseId * EditingCourse.Data
  | EditingTitle of CourseId * CourseTitle * EditingTitle.Data

type CommandGetter<'c> = unit -> Option<'c>

type BotCommands =
  { getInactive: CommandGetter<Inactive.Command>
    getIdle: CommandGetter<Idle.Command>
    getCreatingCourse: CommandGetter<CreatingCourse.Command>
    getEditingCourse: CommandGetter<EditingCourse.Command>
    getEditingTitle: CommandGetter<EditingTitle.Command> }

type Service<'p, 'a> = ('p -> 'a) -> 'a

type BotServices<'a> =
  { tryCreateCourse: CourseTitle -> Service<CourseId option, 'a>
    tryUpdateTitle: CourseId -> CourseTitle -> Service<bool, 'a>
    getCourseTitle: CourseId -> Service<CourseTitle, 'a> }

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
  | Some Idle.Help ->
    callback <| Idle Idle.Helping

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
        callback <| EditingCourse (courseId, EditingCourse.CourseCreated)

      | None ->
        callback <| CreatingCourse CreatingCourse.TitleReserved )
    |> services.tryCreateCourse title

  | None ->
    callback <| CreatingCourse CreatingCourse.Error

let private updateEditingCourse services callback courseId =
  function
  | Some EditingCourse.EditTitle ->
    ( fun courseTitle ->
      (courseId, courseTitle, EditingTitle.Started)
      |> EditingTitle
      |> callback )
    |> services.getCourseTitle courseId

  | Some EditingCourse.Exit ->
    callback <| Idle Idle.ExitedEditing

  | None ->
    callback <| EditingCourse (courseId, EditingCourse.Error)

let private updateEditingTitle services callback courseId courseTitle =
  function
  | Some EditingTitle.Cancel ->
    callback <| EditingCourse (courseId, EditingCourse.TitleCanceled)

  | Some (EditingTitle.SetTitle title) ->
    ( function
      | true ->
        EditingCourse (courseId, EditingCourse.TitleSet)
        |> callback

      | false ->
        EditingTitle (courseId, courseTitle, EditingTitle.TitleReserved)
        |> callback )

    |> services.tryUpdateTitle courseId title

  | None ->
    callback <| EditingTitle (courseId, courseTitle, EditingTitle.Error)

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

  | EditingCourse (courseId, _) ->
    commands.getEditingCourse ()
    |> updateEditingCourse services callback courseId

  | EditingTitle (courseId, courseTitle, _) ->
    commands.getEditingTitle ()
    |> updateEditingTitle services callback courseId courseTitle
