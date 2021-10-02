module InsightClub.Creator.Bot.Core


// Types
type CourseId = int
type CourseTitle = string
type CourseDesc = string

module Inactive =
  type Command = Start

module Idle =
  type Command = Help | CreateCourse
  type Msg =
    | Started
    | Helping
    | CourseCanceled
    | ExitedEditing
    | Error

module CreatingCourse =
  type Command = Cancel | CreateCourse of CourseTitle
  type Msg = Started | TitleReserved | Error

module EditingCourse =
  type Command = EditTitle | EditDesc | Exit
  type Msg =
    | CourseCreated
    | TitleCanceled
    | TitleSet
    | DescCanceled
    | DescSet
    | Error

module EditingTitle =
  type Command = Cancel | SetTitle of CourseTitle
  type Msg = Started | TitleReserved | Error

module EditingDesc =
  type Command = Show | Cancel | SetDesc of CourseDesc
  type Msg = Started | Error

type BotState =
  | Inactive
  | Idle of Idle.Msg
  | CreatingCourse of CreatingCourse.Msg
  | EditingCourse of CourseId * EditingCourse.Msg
  | EditingTitle of CourseId * CourseTitle * EditingTitle.Msg
  | EditingDesc of CourseId * EditingDesc.Msg

type BotIntent =
  | Nothing
  | ShowDesc of CourseDesc

type CommandGetter<'c> = unit -> Option<'c>

type BotCommands =
  { getInactive: CommandGetter<Inactive.Command>
    getIdle: CommandGetter<Idle.Command>
    getCreatingCourse: CommandGetter<CreatingCourse.Command>
    getEditingCourse: CommandGetter<EditingCourse.Command>
    getEditingTitle: CommandGetter<EditingTitle.Command>
    getEditingDesc: CommandGetter<EditingDesc.Command> }

type Service<'p, 'a> = ('p -> 'a) -> 'a

type BotServices<'a> =
  { tryCreateCourse: CourseTitle -> Service<CourseId option, 'a>
    tryUpdateTitle: CourseId -> CourseTitle -> Service<bool, 'a>
    getCourseTitle: CourseId -> Service<CourseTitle, 'a>
    getCourseDesc: CourseId -> Service<CourseDesc, 'a>
    updateDesc: CourseId -> CourseDesc -> Service<unit, 'a> }

// Values
/// Initial state
let initial = Inactive

/// Pairs two values in one tuple
let private (&>) x y = (x, y)

let private updateInactive callback =
  let callback x = callback (x, Nothing)
  function
  | Some Inactive.Start ->
    Idle Idle.Started
    |> callback

  | None ->
    Inactive
    |> callback

let private updateIdle callback =
  let callback x = callback (x, Nothing)
  function
  | Some Idle.Help ->
    Idle Idle.Helping
    |> callback

  | Some Idle.CreateCourse ->
    CreatingCourse CreatingCourse.Started
    |> callback

  | None ->
    Idle Idle.Error
    |> callback

let private updateCreatingCourse services callback =
  let callback x = callback (x, Nothing)
  function
  | Some CreatingCourse.Cancel ->
    Idle Idle.CourseCanceled
    |> callback

  | Some (CreatingCourse.CreateCourse title) ->
    ( function
      | Some courseId ->
        EditingCourse (courseId, EditingCourse.CourseCreated)
        |> callback

      | None ->
        CreatingCourse CreatingCourse.TitleReserved
        |> callback )
    |> services.tryCreateCourse title

  | None ->
    CreatingCourse CreatingCourse.Error
    |> callback

let private updateEditingCourse services callback courseId =
  let callback x = callback (x, Nothing)
  function
  | Some EditingCourse.EditTitle ->
    ( fun courseTitle ->
      (courseId, courseTitle, EditingTitle.Started)
      |> EditingTitle
      |> callback )
    |> services.getCourseTitle courseId

  | Some EditingCourse.EditDesc ->
    EditingDesc (courseId, EditingDesc.Started)
    |> callback

  | Some EditingCourse.Exit ->
    Idle Idle.ExitedEditing
    |> callback

  | None ->
    EditingCourse (courseId, EditingCourse.Error)
    |> callback

let private updateEditingTitle services callback courseId courseTitle =
  let callback x = callback (x, Nothing)
  function
  | Some EditingTitle.Cancel ->
    EditingCourse (courseId, EditingCourse.TitleCanceled)
    |> callback

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
    EditingTitle (courseId, courseTitle, EditingTitle.Error)
    |> callback

let private updateEditingDesc services callback courseId =
  function
  | Some EditingDesc.Show ->
    ( fun desc ->
        EditingDesc (courseId, EditingDesc.Started)
        &> ShowDesc desc
        |> callback )
    |> services.getCourseDesc courseId

  | Some EditingDesc.Cancel ->
    EditingCourse (courseId, EditingCourse.DescCanceled)
    &> Nothing
    |> callback

  | Some (EditingDesc.SetDesc desc) ->
    ( fun () ->
        EditingCourse (courseId, EditingCourse.DescSet)
        &> Nothing
        |> callback )
    |> services.updateDesc courseId desc

  | None ->
    EditingDesc (courseId, EditingDesc.Error)
    &> Nothing
    |> callback

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

  | EditingDesc (courseId, _) ->
    commands.getEditingDesc ()
    |> updateEditingDesc services callback courseId
