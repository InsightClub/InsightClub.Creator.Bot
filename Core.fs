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

type BotEffect =
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
  { callback: BotState * BotEffect -> 'a
    tryCreateCourse: CourseTitle -> Service<CourseId option, 'a>
    tryUpdateTitle: CourseId -> CourseTitle -> Service<bool, 'a>
    getCourseTitle: CourseId -> Service<CourseTitle, 'a>
    getCourseDesc: CourseId -> Service<CourseDesc, 'a>
    updateDesc: CourseId -> CourseDesc -> Service<unit, 'a>
    checkAnyCourse: Service<bool, 'a>
    getCoursesCount: Service<Count, 'a> }

// Values
/// Initial state
let initial = Inactive

let private updateInactive services =
  let callback x = services.callback (x, Nothing)
  function
  | Some Inactive.Start ->
    Idle Idle.Started
    |> callback

  | None ->
    Inactive
    |> callback

let private updateIdle services =
  let callback x = services.callback (x, Nothing)
  function
  | Some Idle.Help ->
    Idle Idle.Helping
    |> callback

  | Some Idle.CreateCourse ->
    CreatingCourse CreatingCourse.Started
    |> callback

  | Some (Idle.EditCourse count) ->
    services.checkAnyCourse <|
      fun any ->
        callback <|
          if any then
            ListingCourses (0, count, ListingCourses.Started)
          else
            Idle Idle.NoCourses

  | None ->
    Idle Idle.Error
    |> callback

let private updateCreatingCourse services =
  let callback x = services.callback (x, Nothing)
  function
  | Some CreatingCourse.Cancel ->
    Idle Idle.CreateCanceled
    |> callback

  | Some (CreatingCourse.CreateCourse title) ->
    services.tryCreateCourse title <|
      fun courseId ->
        callback <|
          match courseId with
          | Some courseId ->
            EditingCourse (courseId, EditingCourse.CourseCreated)

          | None ->
            CreatingCourse CreatingCourse.TitleReserved

  | None ->
    CreatingCourse CreatingCourse.Error
    |> callback

let private updateEditingCourse services courseId =
  let callback x = services.callback (x, Nothing)
  function
  | Some EditingCourse.EditTitle ->
    services.getCourseTitle courseId <|
      fun courseTitle ->
        EditingTitle (courseId, courseTitle, EditingTitle.Started)
        |> callback

  | Some EditingCourse.EditDesc ->
    EditingDesc (courseId, EditingDesc.Started)
    |> callback

  | Some EditingCourse.Exit ->
    Idle Idle.ExitedEditing
    |> callback

  | None ->
    EditingCourse (courseId, EditingCourse.Error)
    |> callback

let private updateEditingTitle services courseId courseTitle =
  let callback x = services.callback (x, Nothing)
  function
  | Some EditingTitle.Cancel ->
    EditingCourse (courseId, EditingCourse.TitleCanceled)
    |> callback

  | Some (EditingTitle.SetTitle title) ->
    services.tryUpdateTitle courseId title <|
      fun isOk ->
        callback <|
          if isOk then
            EditingCourse (courseId, EditingCourse.TitleSet)
          else
            EditingTitle
              ( courseId,
                courseTitle,
                EditingTitle.TitleReserved )

  | None ->
    EditingTitle (courseId, courseTitle, EditingTitle.Error)
    |> callback

let private updateEditingDesc services courseId =
  let callback = services.callback
  function
  | Some EditingDesc.Show ->
    services.getCourseDesc courseId <|
      fun desc ->
        EditingDesc (courseId, EditingDesc.Started)
        &> ShowDesc desc
        |> callback

  | Some EditingDesc.Cancel ->
    EditingCourse (courseId, EditingCourse.DescCanceled)
    &> Nothing
    |> callback

  | Some (EditingDesc.SetDesc desc) ->
    services.updateDesc courseId desc <|
      fun () ->
        EditingCourse (courseId, EditingCourse.DescSet)
        &> Nothing
        |> callback

  | None ->
    EditingDesc (courseId, EditingDesc.Error)
    &> Nothing
    |> callback

let private updateListingCourses services page count =
  let callback = services.callback
  function
  | Some (ListingCourses.Select courseId) ->
    EditingCourse (courseId, EditingCourse.Editing)
    &> Nothing
    |> callback

  | Some ListingCourses.Prev ->
    callback <|
      if page = 0 then
        ListingCourses (page, count, ListingCourses.Started)
        &> InformNoPrev
      else
        ListingCourses (page - 1, count, ListingCourses.Started)
        &> Nothing

  | Some ListingCourses.Next ->
    services.getCoursesCount <|
      fun coursesCount ->
        callback <|
          if (page + 1) * count >= coursesCount then
            ListingCourses (page, count, ListingCourses.Started)
            &> InformNoNext
          else
            ListingCourses (page + 1, count, ListingCourses.Started)
            &> Nothing

  | Some ListingCourses.Exit ->
    Idle Idle.EditCanceled
    &> Nothing
    |> callback

  | None ->
    ListingCourses (page, count, ListingCourses.Error)
    &> Nothing
    |> callback

let update services commands =
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive services

  | Idle _ ->
    commands.getIdle ()
    |> updateIdle services

  | CreatingCourse _ ->
    commands.getCreatingCourse ()
    |> updateCreatingCourse services

  | EditingCourse (courseId, _) ->
    commands.getEditingCourse ()
    |> updateEditingCourse services courseId

  | EditingTitle (courseId, courseTitle, _) ->
    commands.getEditingTitle ()
    |> updateEditingTitle services courseId courseTitle

  | EditingDesc (courseId, _) ->
    commands.getEditingDesc ()
    |> updateEditingDesc services courseId

  | ListingCourses (page, count, _) ->
    commands.getListingCourses ()
    |> updateListingCourses services page count
