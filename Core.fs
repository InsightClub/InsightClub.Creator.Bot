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

    member this.Content =
      match this with
      | Text text        -> text
      | Photo fileId     -> fileId
      | Audio fileId     -> fileId
      | Video fileId     -> fileId
      | Voice fileId     -> fileId
      | Document fileId  -> fileId
      | VideoNote fileId -> fileId

type TitleError =
  | NonUnique
  | TooLong

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
    | TitleError of TitleError
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
    | TitleError of TitleError
    | Error

module EditingDesc =
  type Command<'Effect> =
    | Cancel
    | SetDesc of CourseDesc

  type Msg =
    | Started
    | DescTooLong
    | Error

module ListingCourses =
  type Command<'Effect> =
    | Select of CourseId
    | Prev of informMin: 'Effect
    | Next of informMax: 'Effect
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
    | Prev of informMin: 'Effect
    | Next of informMax: 'Effect
    | Show of showContent: (Content list -> 'Effect)
    | Clean of informEmpty: 'Effect
    | AddContent of Content

  type Msg =
    | Started
    | ContentAdded of Content
    | Cleaned
    | Error

module ListingBlocks =
  type Command<'Effect> =
    | Select of BlockId
    | Prev of informMin: 'Effect
    | Next of informMax: 'Effect
    | Back

  type Msg =
    | Started
    | Error

type BotState =
  | Inactive
  | Idle of Idle.Msg
  | CreatingCourse of CreatingCourse.Msg
  | EditingCourse of CourseId * EditingCourse.Msg
  | EditingTitle of CourseId * EditingTitle.Msg
  | EditingDesc of CourseId * EditingDesc.Msg
  | ListingCourses of Page * Count * ListingCourses.Msg
  | CreatingBlock of CourseId * Index * CreatingBlock.Msg
  | EditingBlock of CourseId * BlockId * Index * BlockTitle * EditingBlock.Msg
  | ListingBlocks of CourseId * Page * Count * ListingBlocks.Msg

type BotPort<'Command> = unit -> 'Command option

type BotDispatcher<'Effect> =
  { askInactive: BotPort<Inactive.Command>
    askIdle: BotPort<Idle.Command>
    askCreatingCourse: BotPort<CreatingCourse.Command>
    askEditingCourse: BotPort<EditingCourse.Command>
    askEditingTitle: BotPort<EditingTitle.Command>
    askEditingDesc: BotPort<EditingDesc.Command<'Effect>>
    askListingCourses: BotPort<ListingCourses.Command<'Effect>>
    askCreatingBlock: BotPort<CreatingBlock.Command>
    askEditingBlock: BotPort<EditingBlock.Command<'Effect>>
    askListingBlocks: BotPort<ListingBlocks.Command<'Effect>> }

type Service<'Param, 'Result> = ('Param -> 'Result) -> 'Result
type Service<'Result> = Service<unit, 'Result>

type BotServices<'Effect, 'Result> =
  { callback: BotState -> 'Effect option -> 'Result
    tryCreateCourse:
      CourseTitle -> Service<Result<CourseId, TitleError>, 'Result>
    tryUpdateTitle:
      CourseId -> CourseTitle -> Service<Result<unit, TitleError>, 'Result>
    tryUpdateDesc: CourseId -> CourseDesc -> Service<bool, 'Result>
    checkAnyCourses: Service<bool, 'Result>
    getCoursesCount: Service<Count, 'Result>
    tryCreateBlock:
      CourseId -> Index -> BlockTitle -> Service<BlockId option, 'Result>
    addContent: BlockId -> Content -> Service<'Result>
    getBlockInfo: BlockId -> Service<Index * BlockTitle, 'Result>
    getBlocksCount: CourseId -> Service<Count, 'Result>
    checkAnyBlocks: CourseId -> Service<bool, 'Result>
    getBlockContents: BlockId -> Service<Content list, 'Result>
    getBlockInfoByIndex:
      CourseId -> Index -> Service<BlockId * BlockTitle, 'Result>
    cleanBlock: BlockId -> Service<bool, 'Result> }

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
    | Ok courseId ->
      callback (EditingCourse (courseId, EditingCourse.CourseCreated)) None

    | Error error ->
      let newState =
        CreatingCourse (CreatingCourse.TitleError error)

      callback newState None

| None ->
  callback (CreatingCourse CreatingCourse.Error) None

let private updateEditingCourse
  callback getBlocksCount checkAnyBlock courseId = function
| Some EditingCourse.EditTitle ->
    let newState =
      EditingTitle (courseId, EditingTitle.Started)

    callback newState None

| Some EditingCourse.EditDesc ->
  callback (EditingDesc (courseId, EditingDesc.Started)) None

| Some EditingCourse.Exit ->
  callback (Idle Idle.ExitedEditing) None

| Some EditingCourse.AddBlock ->
  getBlocksCount courseId <|
    fun count ->
      callback
        (CreatingBlock (courseId, count, CreatingBlock.Started))
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
  callback tryUpdateTitle courseId = function
| Some EditingTitle.Cancel ->
  callback (EditingCourse (courseId, EditingCourse.TitleCanceled)) None

| Some (EditingTitle.SetTitle title) ->
  tryUpdateTitle courseId title <|
    function
    | Ok () ->
      let newState =
        EditingCourse (courseId, EditingCourse.TitleSet)

      callback newState None

    | Error error ->
      let newState =
        EditingTitle (courseId, EditingTitle.TitleError error)

      callback newState None

| None ->
  let newState =
    EditingTitle (courseId, EditingTitle.Error)

  callback newState None

let private updateEditingDesc
  callback tryUpdateDesc courseId = function
| Some EditingDesc.Cancel ->
  callback (EditingCourse (courseId, EditingCourse.DescCanceled)) None

| Some (EditingDesc.SetDesc desc) ->
  tryUpdateDesc courseId desc <|
    function
    | true ->
      let newState =
        EditingCourse (courseId, EditingCourse.DescSet)

      callback newState None

    | false ->
      let newState =
        EditingDesc (courseId, EditingDesc.DescTooLong)

      callback newState None

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
  callback tryCreateBlock courseId blockIndex = function
| Some CreatingBlock.Cancel ->
  callback (EditingCourse (courseId, EditingCourse.NewBlockCanceled)) None

| Some (CreatingBlock.CreateBlock title) ->
  tryCreateBlock courseId blockIndex title <|
    function
    | Some blockId ->
      callback
        ( EditingBlock
            ( courseId,
              blockId,
              blockIndex,
              title,
              EditingBlock.Started ) )
        None

    | None ->
      callback
        (CreatingBlock (courseId, blockIndex, CreatingBlock.TitleReserved))
        None

| None ->
  callback (CreatingBlock (courseId, blockIndex, CreatingBlock.Error)) None

let private updateEditingBlock
  callback
  addContent
  getBlockContents
  getBlockInfoByIndex
  getBlocksCount
  cleanBlock
  courseId
  blockId
  blockIndex
  title
  = function
| Some EditingBlock.Back ->
  callback (EditingCourse (courseId, EditingCourse.Editing)) None

| Some EditingBlock.Nothing ->
  callback
    ( EditingBlock
        ( courseId,
          blockId,
          blockIndex,
          title,
          EditingBlock.Started) )
    None

| Some EditingBlock.InsertBefore ->
  callback
    (CreatingBlock (courseId, blockIndex, CreatingBlock.Started))
    None

| Some EditingBlock.InsertAfter ->
  callback
    (CreatingBlock (courseId, blockIndex + 1, CreatingBlock.Started))
    None

| Some (EditingBlock.Prev informMin) ->
  if blockIndex = 0 then
    callback
      ( EditingBlock
          ( courseId,
            blockId,
            blockIndex,
            title,
            EditingBlock.Started ) )
      (Some informMin)
  else
    getBlockInfoByIndex courseId (blockIndex - 1) <|
      fun (blockId, title) ->
        callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex - 1,
                title,
                EditingBlock.Started ) )
          None

| Some (EditingBlock.Next informMax) ->
  getBlocksCount courseId <|
    fun count ->
      if blockIndex = count - 1 then
        callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex,
                title,
                EditingBlock.Started ) )
          (Some informMax)
      else
        getBlockInfoByIndex courseId (blockIndex + 1) <|
          fun (blockId, title) ->
            callback
              ( EditingBlock
                  ( courseId,
                    blockId,
                    blockIndex + 1,
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
              blockIndex,
              title,
              EditingBlock.Started ) )
        (Some <| show contents)
| Some (EditingBlock.Clean informEmpty) ->
  cleanBlock blockId <|
    fun cleaned ->
      if cleaned then
        callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex,
                title,
                EditingBlock.Cleaned ) )
          None
      else
        callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex,
                title,
                EditingBlock.Cleaned ) )
          (Some <| informEmpty)

| Some (EditingBlock.AddContent content) ->
  addContent blockId content <|
    fun () ->
      callback
        ( EditingBlock
            ( courseId,
              blockId,
              blockIndex,
              title,
              EditingBlock.ContentAdded content ) )
        None

| None ->
  callback
    ( EditingBlock
        ( courseId,
          blockId,
          blockIndex,
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

let update services dispatcher =
  let s = services
  function
  | Inactive ->
    dispatcher.askInactive ()
    |> updateInactive s.callback

  | Idle _ ->
    dispatcher.askIdle ()
    |> updateIdle s.callback s.checkAnyCourses

  | CreatingCourse _ ->
    dispatcher.askCreatingCourse ()
    |> updateCreatingCourse s.callback s.tryCreateCourse

  | EditingCourse (courseId, _) ->
    dispatcher.askEditingCourse ()
    |> updateEditingCourse
      s.callback s.getBlocksCount s.checkAnyBlocks courseId

  | EditingTitle (courseId, _) ->
    dispatcher.askEditingTitle ()
    |> updateEditingTitle s.callback s.tryUpdateTitle courseId

  | EditingDesc (courseId, _) ->
    dispatcher.askEditingDesc ()
    |> updateEditingDesc s.callback s.tryUpdateDesc courseId

  | ListingCourses (page, count, _) ->
    dispatcher.askListingCourses ()
    |> updateListingCourses s.callback s.getCoursesCount page count

  | CreatingBlock (courseId, lastIndex, _) ->
    dispatcher.askCreatingBlock ()
    |> updateCreatingBlock s.callback s.tryCreateBlock courseId lastIndex

  | EditingBlock (courseId, blockId, index, title, _) ->
    dispatcher.askEditingBlock ()
    |> updateEditingBlock
      s.callback
      s.addContent
      s.getBlockContents
      s.getBlockInfoByIndex
      s.getBlocksCount
      s.cleanBlock
      courseId
      blockId
      index
      title

  | ListingBlocks (courseId, page, count, _) ->
    dispatcher.askListingBlocks ()
    |> updateListingBlocks
      s.callback s.getBlockInfo s.getBlocksCount courseId page count
