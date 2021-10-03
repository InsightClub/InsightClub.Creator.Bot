module InsightClub.Creator.Bot.Core


// Types
type CourseId = int
type CourseTitle = string
type CourseDesc = string
type Page = int
type Count = int

module Inactive =
  type Command = Start

module Idle =
  type Command =
    | Help
    | CreateCourse
    | EditCourse of Count

  type Msg =
    | Started
    | Helping
    | NoCourses
    | CreateCanceled
    | EditCanceled
    | ExitedEditing
    | Error

module CreatingCourse =
  type Command =
    | Cancel
    | CreateCourse of CourseTitle

  type Msg =
    | Started
    | TitleReserved
    | Error

module EditingCourse =
  type Command =
    | EditTitle
    | EditDesc
    | Exit

  type Msg =
    | CourseCreated
    | Editing
    | TitleCanceled
    | TitleSet
    | DescCanceled
    | DescSet
    | Error

module EditingTitle =
  type Command =
    | Cancel
    | SetTitle of CourseTitle

  type Msg =
    | Started
    | TitleReserved
    | Error

module EditingDesc =
  type Command =
    | Show
    | Cancel
    | SetDesc of CourseDesc

  type Msg =
    | Started
    | Error

module ListingCourses =
  type Command =
    | Select of CourseId
    | Prev
    | Next
    | Exit

  type Msg =
    | Started
    | Error

type BotState =
  | Inactive
  | Idle of Idle.Msg
  | CreatingCourse of CreatingCourse.Msg
  | EditingCourse of CourseId * EditingCourse.Msg
  | EditingTitle of CourseId * CourseTitle * EditingTitle.Msg
  | EditingDesc of CourseId * EditingDesc.Msg
  | ListingCourses of Page * Count * ListingCourses.Msg

type BotIntent =
  | Nothing
  | ShowDesc of CourseDesc
  | InformNoPrev
  | InformNoNext

type CommandGetter<'c> = unit -> Option<'c>

type BotCommands =
  { getInactive: CommandGetter<Inactive.Command>
    getIdle: CommandGetter<Idle.Command>
    getCreatingCourse: CommandGetter<CreatingCourse.Command>
    getEditingCourse: CommandGetter<EditingCourse.Command>
    getEditingTitle: CommandGetter<EditingTitle.Command>
    getEditingDesc: CommandGetter<EditingDesc.Command>
    getListingCourses: CommandGetter<ListingCourses.Command> }

type Service<'p, 'a> = ('p -> 'a) -> 'a

type BotServices<'a> =
  { tryCreateCourse: CourseTitle -> Service<CourseId option, 'a>
    tryUpdateTitle: CourseId -> CourseTitle -> Service<bool, 'a>
    getCourseTitle: CourseId -> Service<CourseTitle, 'a>
    getCourseDesc: CourseId -> Service<CourseDesc, 'a>
    updateDesc: CourseId -> CourseDesc -> Service<unit, 'a>
    checkAnyCourse: Service<bool, 'a>
    getCoursesCount: Service<Count, 'a> }

// Values
/// Initial state
let initial = Inactive

let private updateInactive callback =
  let callback x = callback (x, Nothing)
  function
  | Some Inactive.Start ->
    Idle Idle.Started
    |> callback

  | None ->
    Inactive
    |> callback

let private updateIdle services callback =
  let callback x = callback (x, Nothing)
  function
  | Some Idle.Help ->
    Idle Idle.Helping
    |> callback

  | Some Idle.CreateCourse ->
    CreatingCourse CreatingCourse.Started
    |> callback

  | Some (Idle.EditCourse count) ->
    ( function
      | true ->
        ListingCourses (0, count, ListingCourses.Started)
        |> callback

      | false ->
        Idle Idle.NoCourses
        |> callback )
    |> services.checkAnyCourse

  | None ->
    Idle Idle.Error
    |> callback

let private updateCreatingCourse services callback =
  let callback x = callback (x, Nothing)
  function
  | Some CreatingCourse.Cancel ->
    Idle Idle.CreateCanceled
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

let private updateListingCourses services callback page count =
  function
  | Some (ListingCourses.Select courseId) ->
    EditingCourse (courseId, EditingCourse.Editing)
    &> Nothing
    |> callback

  | Some ListingCourses.Prev ->
      if page = 0 then
        ListingCourses (page, count, ListingCourses.Started)
        &> InformNoPrev
      else
        ListingCourses (page - 1, count, ListingCourses.Started)
        &> Nothing
    |> callback

  | Some ListingCourses.Next ->
    ( fun coursesCount ->
        if (page + 1) * count >= coursesCount then
          ListingCourses (page, count, ListingCourses.Started)
          &> InformNoNext
        else
          ListingCourses (page + 1, count, ListingCourses.Started)
          &> Nothing
      |> callback )
    |> services.getCoursesCount

  | Some ListingCourses.Exit ->
    Idle Idle.EditCanceled
    &> Nothing
    |> callback

  | None ->
    ListingCourses (page, count, ListingCourses.Error)
    &> Nothing
    |> callback

let update services commands callback =
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive callback

  | Idle _ ->
    commands.getIdle ()
    |> updateIdle services callback

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

  | ListingCourses (page, count, _) ->
    commands.getListingCourses ()
    |> updateListingCourses services callback page count
