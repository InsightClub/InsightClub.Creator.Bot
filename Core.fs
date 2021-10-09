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
  type Command<'Effect> =
    | Show of (CourseDesc -> 'Effect)
    | Cancel
    | SetDesc of CourseDesc

  type Msg =
    | Started
    | Error

module ListingCourses =
  type Command<'Effect> =
    | Select of CourseId
    | Prev of 'Effect
    | Next of 'Effect
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

type GetCommand<'Command> = unit -> Option<'Command>

type BotCommands<'Effect> =
  { getInactive: GetCommand<Inactive.Command>
    getIdle: GetCommand<Idle.Command>
    getCreatingCourse: GetCommand<CreatingCourse.Command>
    getEditingCourse: GetCommand<EditingCourse.Command>
    getEditingTitle: GetCommand<EditingTitle.Command>
    getEditingDesc: GetCommand<EditingDesc.Command<'Effect>>
    getListingCourses: GetCommand<ListingCourses.Command<'Effect>> }

type Service<'Param, 'Result> = ('Param -> 'Result) -> 'Result

type BotServices<'Effect, 'Result> =
  { callback: BotState -> 'Effect option -> 'Result
    tryCreateCourse: CourseTitle -> Service<CourseId option, 'Result>
    tryUpdateTitle: CourseId -> CourseTitle -> Service<bool, 'Result>
    getCourseTitle: CourseId -> Service<CourseTitle, 'Result>
    getCourseDesc: CourseId -> Service<CourseDesc, 'Result>
    updateDesc: CourseId -> CourseDesc -> Service<unit, 'Result>
    checkAnyCourse: Service<bool, 'Result>
    getCoursesCount: Service<Count, 'Result> }

// Values
/// Initial state
let initial = Inactive

let private updateInactive callback = function
| Some Inactive.Start ->
  callback (Idle Idle.Started) None

| None ->
  callback Inactive None

let private updateIdle callback checkAnyCourse = function
| Some Idle.Help ->
  callback (Idle Idle.Helping) None

| Some Idle.CreateCourse ->
  callback (CreatingCourse CreatingCourse.Started) None

| Some (Idle.EditCourse count) ->
  checkAnyCourse <|
    fun any ->
      let state =
        if any then
          ListingCourses (0, count, ListingCourses.Started)
        else
          Idle Idle.NoCourses
      callback state None

| None ->
  callback (Idle Idle.Error) None

let private updateCreatingCourse callback tryCreateCourse = function
| Some CreatingCourse.Cancel ->
  callback (Idle Idle.CreateCanceled) None

| Some (CreatingCourse.CreateCourse title) ->
  tryCreateCourse title <|
    function
    | Some courseId ->
      callback (EditingCourse (courseId, EditingCourse.CourseCreated)) None

    | None ->
      callback (CreatingCourse CreatingCourse.TitleReserved) None

| None ->
  callback (CreatingCourse CreatingCourse.Error) None

let private updateEditingCourse callback getCourseTitle courseId = function
| Some EditingCourse.EditTitle ->
  getCourseTitle courseId <|
    fun courseTitle ->
      callback (EditingTitle (courseId, courseTitle, EditingTitle.Started)) None

| Some EditingCourse.EditDesc ->
  callback (EditingDesc (courseId, EditingDesc.Started)) None

| Some EditingCourse.Exit ->
  callback (Idle Idle.ExitedEditing) None

| None ->
  callback (EditingCourse (courseId, EditingCourse.Error)) None

let private updateEditingTitle
  callback tryUpdateTitle courseId courseTitle = function
| Some EditingTitle.Cancel ->
  callback (EditingCourse (courseId, EditingCourse.TitleCanceled)) None

| Some (EditingTitle.SetTitle title) ->
  tryUpdateTitle courseId title <|
    function
    | true ->
      callback
        (EditingCourse (courseId, EditingCourse.TitleSet))
        None

    | false ->
      callback
        ( EditingTitle
            ( courseId,
              courseTitle,
              EditingTitle.TitleReserved ) )
        None

| None ->
  callback (EditingTitle (courseId, courseTitle, EditingTitle.Error)) None

let private updateEditingDesc
  callback getCourseDesc updateDesc courseId = function
| Some (EditingDesc.Show show) ->
  getCourseDesc courseId <|
    fun desc ->
      callback (EditingDesc (courseId, EditingDesc.Started)) (Some <| show desc)

| Some EditingDesc.Cancel ->
  callback (EditingCourse (courseId, EditingCourse.DescCanceled)) None

| Some (EditingDesc.SetDesc desc) ->
  updateDesc courseId desc <|
    fun () ->
      callback (EditingCourse (courseId, EditingCourse.DescSet)) None

| None ->
  callback (EditingDesc (courseId, EditingDesc.Error)) None

let private updateListingCourses callback getCoursesCount page count = function
| Some (ListingCourses.Select courseId) ->
  callback (EditingCourse (courseId, EditingCourse.Editing)) None

| Some (ListingCourses.Prev informMin) ->
  let state, effect =
    if page = 0 then
      ListingCourses (page, count, ListingCourses.Started), Some informMin
    else
      ListingCourses (page - 1, count, ListingCourses.Started), None

  callback state effect

| Some (ListingCourses.Next informMax) ->
  getCoursesCount <|
    fun coursesCount ->
      let state, effect =
        if (page + 1) * count >= coursesCount then
          ListingCourses (page, count, ListingCourses.Started), Some informMax
        else
          ListingCourses (page + 1, count, ListingCourses.Started), None

      callback state effect

| Some ListingCourses.Exit ->
  callback (Idle Idle.EditCanceled) None

| None ->
  callback (ListingCourses (page, count, ListingCourses.Error)) None

let update services commands =
  let s = services
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive s.callback

  | Idle _ ->
    commands.getIdle ()
    |> updateIdle s.callback s.checkAnyCourse

  | CreatingCourse _ ->
    commands.getCreatingCourse ()
    |> updateCreatingCourse s.callback s.tryCreateCourse

  | EditingCourse (courseId, _) ->
    commands.getEditingCourse ()
    |> updateEditingCourse s.callback s.getCourseTitle courseId

  | EditingTitle (courseId, courseTitle, _) ->
    commands.getEditingTitle ()
    |> updateEditingTitle s.callback s.tryUpdateTitle courseId courseTitle

  | EditingDesc (courseId, _) ->
    commands.getEditingDesc ()
    |> updateEditingDesc s.callback s.getCourseDesc s.updateDesc courseId

  | ListingCourses (page, count, _) ->
    commands.getListingCourses ()
    |> updateListingCourses s.callback s.getCoursesCount page count
