module InsightClub.Creator.Bot.Core


// Types
type CourseId = int
type BlockId = int
type CourseTitle = string
type CourseDesc = string
type BlockTitle = string
type Page = int
type Count = int
type Index = int
type Text = string
type FileId = string

type Content =
  | Text of Text
  | Photo of FileId
  | Audio of FileId
  | Video of FileId
  | Voice of FileId
  | Document of FileId
  | VideoNote of FileId
  with
    member this.IsFile =
      match this with
      | Text _ -> false
      | _      -> true

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
    | AddBlock
    | EditBlock of Count
    | Exit

  type Msg =
    | CourseCreated
    | Editing
    | TitleCanceled
    | TitleSet
    | DescCanceled
    | DescSet
    | NewBlockCanceled
    | BlockCanceled
    | NoBlocks
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

module CreatingBlock =
  type Command =
    | Cancel
    | CreateBlock of BlockTitle

  type Msg =
    | Started
    | TitleReserved
    | Error

module EditingBlock =
  type Command<'Effect> =
    | Back
    | Nothing
    | InsertBefore
    | InsertAfter
    | Prev of 'Effect
    | Next of 'Effect
    | Show of (Content list -> 'Effect)
    | AddContent of Content

  type Msg =
    | Started
    | ContentAdded of Content
    | Error

module ListingBlocks =
  type Command<'Effect> =
    | Select of BlockId
    | Prev of 'Effect
    | Next of 'Effect
    | Back

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
  | CreatingBlock of CourseId * Index * CreatingBlock.Msg
  | EditingBlock of CourseId * BlockId * Index * BlockTitle * EditingBlock.Msg
  | ListingBlocks of CourseId * Page * Count * ListingBlocks.Msg

type GetCommand<'Command> = unit -> 'Command option

type BotCommands<'Effect> =
  { getInactive: GetCommand<Inactive.Command>
    getIdle: GetCommand<Idle.Command>
    getCreatingCourse: GetCommand<CreatingCourse.Command>
    getEditingCourse: GetCommand<EditingCourse.Command>
    getEditingTitle: GetCommand<EditingTitle.Command>
    getEditingDesc: GetCommand<EditingDesc.Command<'Effect>>
    getListingCourses: GetCommand<ListingCourses.Command<'Effect>>
    getCreatingBlock: GetCommand<CreatingBlock.Command>
    getEditingBlock: GetCommand<EditingBlock.Command<'Effect>>
    getListingBlocks: GetCommand<ListingBlocks.Command<'Effect>> }

type Service<'Param, 'Result> = ('Param -> 'Result) -> 'Result
type Service<'Result> = Service<unit, 'Result>

type BotServices<'Effect, 'Result> =
  { callback: BotState -> 'Effect option -> 'Result
    tryCreateCourse: CourseTitle -> Service<CourseId option, 'Result>
    tryUpdateTitle: CourseId -> CourseTitle -> Service<bool, 'Result>
    getCourseTitle: CourseId -> Service<CourseTitle, 'Result>
    getCourseDesc: CourseId -> Service<CourseDesc, 'Result>
    updateDesc: CourseId -> CourseDesc -> Service<'Result>
    checkAnyCourses: Service<bool, 'Result>
    getCoursesCount: Service<Count, 'Result>
    tryCreateBlock:
      CourseId -> Index -> BlockTitle -> Service<BlockId option, 'Result>
    getLastBlockIndex: CourseId -> Service<Index, 'Result>
    addContent: BlockId -> Content -> Service<'Result>
    getBlockInfo: BlockId -> Service<Index * BlockTitle, 'Result>
    getBlocksCount: CourseId -> Service<Count, 'Result>
    checkAnyBlocks: CourseId -> Service<bool, 'Result>
    getBlockContents: BlockId -> Service<Content list, 'Result>
    getBlockInfoByIndex:
      CourseId -> Index -> Service<BlockId * BlockTitle, 'Result> }

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

let private updateEditingCourse
  callback getCourseTitle getLastBlockIndex checkAnyBlock courseId = function
| Some EditingCourse.EditTitle ->
  getCourseTitle courseId <|
    fun courseTitle ->
      callback
        (EditingTitle (courseId, courseTitle, EditingTitle.Started))
        None

| Some EditingCourse.EditDesc ->
  callback (EditingDesc (courseId, EditingDesc.Started)) None

| Some EditingCourse.Exit ->
  callback (Idle Idle.ExitedEditing) None

| Some EditingCourse.AddBlock ->
  getLastBlockIndex courseId <|
    fun lastIndex ->
      callback
        (CreatingBlock (courseId, lastIndex, CreatingBlock.Started))
        None

| Some (EditingCourse.EditBlock count) ->
  checkAnyBlock courseId <|
    fun any ->
      let state =
        if any then
          ListingBlocks (courseId, 0, count, ListingBlocks.Started)
        else
          EditingCourse (courseId, EditingCourse.NoBlocks)

      callback state None

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
      callback
        (EditingDesc (courseId, EditingDesc.Started))
        (Some <| show desc)

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

let private updateCreatingBlock
  callback tryCreateBlock courseId lastIndex = function
| Some CreatingBlock.Cancel ->
  callback (EditingCourse (courseId, EditingCourse.NewBlockCanceled)) None

| Some (CreatingBlock.CreateBlock title) ->
  tryCreateBlock courseId (lastIndex + 1) title <|
    function
    | Some blockId ->
      callback
        ( EditingBlock
            ( courseId,
              blockId,
              lastIndex + 1,
              title,
              EditingBlock.Started ) )
        None

    | None ->
      callback
        (CreatingBlock (courseId, lastIndex, CreatingBlock.TitleReserved))
        None

| None ->
  callback (CreatingBlock (courseId, lastIndex, CreatingBlock.Error)) None

let private updateEditingBlock
  callback
  addContent
  getBlockContents
  getBlockInfoByIndex
  getBlocksCount
  courseId
  blockId
  index
  title
  = function
| Some EditingBlock.Back ->
  callback (EditingCourse (courseId, EditingCourse.Editing)) None

| Some EditingBlock.Nothing ->
  callback
    ( EditingBlock
        ( courseId,
          blockId,
          index,
          title,
          EditingBlock.Started) )
    None

| Some EditingBlock.InsertBefore ->
  callback (CreatingBlock (courseId, index - 1, CreatingBlock.Started)) None

| Some EditingBlock.InsertAfter ->
  callback (CreatingBlock (courseId, index, CreatingBlock.Started)) None

| Some (EditingBlock.Prev informMin) ->
  if index = 1 then
    callback
      ( EditingBlock
          ( courseId,
            blockId,
            index,
            title,
            EditingBlock.Started ) )
      (Some informMin)
  else
    getBlockInfoByIndex courseId (index - 1) <|
      fun (blockId, title) ->
        callback
          ( EditingBlock
              ( courseId,
                blockId,
                index - 1,
                title,
                EditingBlock.Started ) )
          None

| Some (EditingBlock.Next informMax) ->
  getBlocksCount courseId <|
    fun count ->
      if index = count then
        callback
          ( EditingBlock
              ( courseId,
                blockId,
                index,
                title,
                EditingBlock.Started ) )
          (Some informMax)
      else
        getBlockInfoByIndex courseId (index + 1) <|
          fun (blockId, title) ->
            callback
              ( EditingBlock
                  ( courseId,
                    blockId,
                    index + 1,
                    title,
                    EditingBlock.Started ) )
              None

| Some (EditingBlock.Show show) ->
  getBlockContents blockId <|
    fun contents ->
      callback
        ( EditingBlock
            ( courseId,
              blockId,
              index,
              title,
              EditingBlock.Started ) )
        (Some <| show contents)

| Some (EditingBlock.AddContent content) ->
  addContent blockId content <|
    fun () ->
      callback
        ( EditingBlock
            ( courseId,
              blockId,
              index,
              title,
              EditingBlock.ContentAdded content ) )
        None

| None ->
  callback
    ( EditingBlock
        ( courseId,
          blockId,
          index,
          title,
          EditingBlock.Error ) )
    None

let updateListingBlocks
  callback getBlockInfo getBlocksCount courseId page count = function
| Some (ListingBlocks.Select blockId) ->
  getBlockInfo blockId <|
    fun (index, title) ->
      callback
        ( EditingBlock
            ( courseId,
              blockId,
              index,
              title,
              EditingBlock.Started ) )
        None

| Some (ListingBlocks.Prev informMin) ->
  let state, effect =
    if page = 0 then
      ListingBlocks
        ( courseId,
          page,
          count,
          ListingBlocks.Started ),
        Some informMin
    else
      ListingBlocks
        ( courseId,
          page - 1,
          count,
          ListingBlocks.Started ),
        None

  callback state effect

| Some (ListingBlocks.Next informMax) ->
  getBlocksCount courseId <|
    fun blocksCount ->
      let state, effect =
        if (page + 1) * count >= blocksCount then
          ListingBlocks
            ( courseId,
              page,
              count,
              ListingBlocks.Started ),
            Some informMax
        else
          ListingBlocks
            ( courseId,
              page + 1,
              count,
              ListingBlocks.Started ),
            None

      callback state effect

| Some ListingBlocks.Back ->
  callback (EditingCourse (courseId, EditingCourse.BlockCanceled)) None

| None ->
  callback (ListingBlocks (courseId, page, count, ListingBlocks.Error)) None

let update services commands =
  let s = services
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive s.callback

  | Idle _ ->
    commands.getIdle ()
    |> updateIdle s.callback s.checkAnyCourses

  | CreatingCourse _ ->
    commands.getCreatingCourse ()
    |> updateCreatingCourse s.callback s.tryCreateCourse

  | EditingCourse (courseId, _) ->
    commands.getEditingCourse ()
    |> updateEditingCourse
      s.callback s.getCourseTitle s.getLastBlockIndex s.checkAnyBlocks courseId

  | EditingTitle (courseId, courseTitle, _) ->
    commands.getEditingTitle ()
    |> updateEditingTitle s.callback s.tryUpdateTitle courseId courseTitle

  | EditingDesc (courseId, _) ->
    commands.getEditingDesc ()
    |> updateEditingDesc s.callback s.getCourseDesc s.updateDesc courseId

  | ListingCourses (page, count, _) ->
    commands.getListingCourses ()
    |> updateListingCourses s.callback s.getCoursesCount page count

  | CreatingBlock (courseId, lastIndex, _) ->
    commands.getCreatingBlock ()
    |> updateCreatingBlock s.callback s.tryCreateBlock courseId lastIndex

  | EditingBlock (courseId, blockId, index, title, _) ->
    commands.getEditingBlock ()
    |> updateEditingBlock
      s.callback
      s.addContent
      s.getBlockContents
      s.getBlockInfoByIndex
      s.getBlocksCount
      courseId
      blockId
      index
      title

  | ListingBlocks (courseId, page, count, _) ->
    commands.getListingBlocks ()
    |> updateListingBlocks
      s.callback s.getBlockInfo s.getBlocksCount courseId page count
