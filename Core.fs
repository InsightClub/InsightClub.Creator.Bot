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

let private updateInactive services = function
| Some Inactive.Start ->
  services.callback (Idle Idle.Started) None

| None ->
  services.callback Inactive None

let private updateIdle services = function
| Some Idle.Help ->
  services.callback (Idle Idle.Helping) None

| Some Idle.CreateCourse ->
  services.callback (CreatingCourse CreatingCourse.Started) None

| Some (Idle.EditCourse count) ->
  services.checkAnyCourses <|
    fun any ->
      let state =
        if any then
          ListingCourses (0, count, ListingCourses.Started)
        else
          Idle Idle.NoCourses

      services.callback state None

| None ->
  services.callback (Idle Idle.Error) None

let private updateCreatingCourse services = function
| Some CreatingCourse.Cancel ->
  services.callback (Idle Idle.CreateCanceled) None

| Some (CreatingCourse.CreateCourse title) ->
  services.tryCreateCourse title <|
    function
    | Ok courseId ->
      services.callback
        (EditingCourse (courseId, EditingCourse.CourseCreated))
        None

    | Error error ->
      let newState =
        CreatingCourse (CreatingCourse.TitleError error)

      services.callback newState None

| None ->
  services.callback (CreatingCourse CreatingCourse.Error) None

let private updateEditingCourse services courseId = function
| Some EditingCourse.EditTitle ->
    let newState =
      EditingTitle (courseId, EditingTitle.Started)

    services.callback newState None

| Some EditingCourse.EditDesc ->
  services.callback (EditingDesc (courseId, EditingDesc.Started)) None

| Some EditingCourse.Exit ->
  services.callback (Idle Idle.ExitedEditing) None

| Some EditingCourse.AddBlock ->
  services.getBlocksCount courseId <|
    fun count ->
      services.callback
        (CreatingBlock (courseId, count, CreatingBlock.Started))
        None

| Some (EditingCourse.EditBlock count) ->
  services.checkAnyBlocks courseId <|
    fun any ->
      let state =
        if any then
          ListingBlocks (courseId, 0, count, ListingBlocks.Started)
        else
          EditingCourse (courseId, EditingCourse.NoBlocks)

      services.callback state None

| None ->
  services.callback (EditingCourse (courseId, EditingCourse.Error)) None

let private updateEditingTitle services courseId = function
| Some EditingTitle.Cancel ->
  services.callback (EditingCourse (courseId, EditingCourse.TitleCanceled)) None

| Some (EditingTitle.SetTitle title) ->
  services.tryUpdateTitle courseId title <|
    function
    | Ok () ->
      let newState =
        EditingCourse (courseId, EditingCourse.TitleSet)

      services.callback newState None

    | Error error ->
      let newState =
        EditingTitle (courseId, EditingTitle.TitleError error)

      services.callback newState None

| None ->
  let newState =
    EditingTitle (courseId, EditingTitle.Error)

  services.callback newState None

let private updateEditingDesc services courseId = function
| Some EditingDesc.Cancel ->
  services.callback (EditingCourse (courseId, EditingCourse.DescCanceled)) None

| Some (EditingDesc.SetDesc desc) ->
  services.tryUpdateDesc courseId desc <|
    function
    | true ->
      let newState =
        EditingCourse (courseId, EditingCourse.DescSet)

      services.callback newState None

    | false ->
      let newState =
        EditingDesc (courseId, EditingDesc.DescTooLong)

      services.callback newState None

| None ->
  services.callback (EditingDesc (courseId, EditingDesc.Error)) None

let private updateListingCourses services page count = function
| Some (ListingCourses.Select courseId) ->
  services.callback (EditingCourse (courseId, EditingCourse.Editing)) None

| Some (ListingCourses.Prev informMin) ->
  let state, effect =
    if page = 0 then
      ListingCourses (page, count, ListingCourses.Started), Some informMin
    else
      ListingCourses (page - 1, count, ListingCourses.Started), None

  services.callback state effect

| Some (ListingCourses.Next informMax) ->
  services.getCoursesCount <|
    fun coursesCount ->
      let state, effect =
        if (page + 1) * count >= coursesCount then
          ListingCourses (page, count, ListingCourses.Started), Some informMax
        else
          ListingCourses (page + 1, count, ListingCourses.Started), None

      services.callback state effect

| Some ListingCourses.Exit ->
  services.callback (Idle Idle.EditCanceled) None

| None ->
  services.callback (ListingCourses (page, count, ListingCourses.Error)) None

let private updateCreatingBlock services courseId blockIndex = function
| Some CreatingBlock.Cancel ->
  services.callback
    (EditingCourse (courseId, EditingCourse.NewBlockCanceled))
    None

| Some (CreatingBlock.CreateBlock title) ->
  services.tryCreateBlock courseId blockIndex title <|
    function
    | Some blockId ->
      services.callback
        ( EditingBlock
            ( courseId,
              blockId,
              blockIndex,
              title,
              EditingBlock.Started ) )
        None

    | None ->
      services.callback
        (CreatingBlock (courseId, blockIndex, CreatingBlock.TitleReserved))
        None

| None ->
  services.callback
    (CreatingBlock (courseId, blockIndex, CreatingBlock.Error))
    None

let private updateEditingBlock
  services courseId blockId blockIndex title = function
| Some EditingBlock.Back ->
  services.callback (EditingCourse (courseId, EditingCourse.Editing)) None

| Some EditingBlock.Nothing ->
  services.callback
    ( EditingBlock
        ( courseId,
          blockId,
          blockIndex,
          title,
          EditingBlock.Started) )
    None

| Some EditingBlock.InsertBefore ->
  services.callback
    (CreatingBlock (courseId, blockIndex, CreatingBlock.Started))
    None

| Some EditingBlock.InsertAfter ->
  services.callback
    (CreatingBlock (courseId, blockIndex + 1, CreatingBlock.Started))
    None

| Some (EditingBlock.Prev informMin) ->
  if blockIndex = 0 then
    services.callback
      ( EditingBlock
          ( courseId,
            blockId,
            blockIndex,
            title,
            EditingBlock.Started ) )
      (Some informMin)
  else
    services.getBlockInfoByIndex courseId (blockIndex - 1) <|
      fun (blockId, title) ->
        services.callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex - 1,
                title,
                EditingBlock.Started ) )
          None

| Some (EditingBlock.Next informMax) ->
  services.getBlocksCount courseId <|
    fun count ->
      if blockIndex = count - 1 then
        services.callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex,
                title,
                EditingBlock.Started ) )
          (Some informMax)
      else
        services.getBlockInfoByIndex courseId (blockIndex + 1) <|
          fun (blockId, title) ->
            services.callback
              ( EditingBlock
                  ( courseId,
                    blockId,
                    blockIndex + 1,
                    title,
                    EditingBlock.Started ) )
              None

| Some (EditingBlock.Show show) ->
  services.getBlockContents blockId <|
    fun contents ->
      services.callback
        ( EditingBlock
            ( courseId,
              blockId,
              blockIndex,
              title,
              EditingBlock.Started ) )
        (Some <| show contents)

| Some (EditingBlock.Clean informEmpty) ->
  services.cleanBlock blockId <|
    fun cleaned ->
      if cleaned then
        services.callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex,
                title,
                EditingBlock.Cleaned ) )
          None
      else
        services.callback
          ( EditingBlock
              ( courseId,
                blockId,
                blockIndex,
                title,
                EditingBlock.Cleaned ) )
          (Some <| informEmpty)

| Some (EditingBlock.AddContent content) ->
  services.addContent blockId content <|
    fun () ->
      services.callback
        ( EditingBlock
            ( courseId,
              blockId,
              blockIndex,
              title,
              EditingBlock.ContentAdded content ) )
        None

| None ->
  services.callback
    ( EditingBlock
        ( courseId,
          blockId,
          blockIndex,
          title,
          EditingBlock.Error ) )
    None

let updateListingBlocks services courseId page count = function
| Some (ListingBlocks.Select blockId) ->
  services.getBlockInfo blockId <|
    fun (index, title) ->
      services.callback
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

  services.callback state effect

| Some (ListingBlocks.Next informMax) ->
  services.getBlocksCount courseId <|
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

      services.callback state effect

| Some ListingBlocks.Back ->
  services.callback
    (EditingCourse (courseId, EditingCourse.BlockCanceled))
    None

| None ->
  services.callback
    (ListingBlocks (courseId, page, count, ListingBlocks.Error))
    None

let update services dispatcher = function
| Inactive ->
  dispatcher.askInactive ()
  |> updateInactive services

| Idle _ ->
  dispatcher.askIdle ()
  |> updateIdle services

| CreatingCourse _ ->
  dispatcher.askCreatingCourse ()
  |> updateCreatingCourse services

| EditingCourse (courseId, _) ->
  dispatcher.askEditingCourse ()
  |> updateEditingCourse services courseId

| EditingTitle (courseId, _) ->
  dispatcher.askEditingTitle ()
  |> updateEditingTitle services courseId

| EditingDesc (courseId, _) ->
  dispatcher.askEditingDesc ()
  |> updateEditingDesc services courseId

| ListingCourses (page, count, _) ->
  dispatcher.askListingCourses ()
  |> updateListingCourses services page count

| CreatingBlock (courseId, lastIndex, _) ->
  dispatcher.askCreatingBlock ()
  |> updateCreatingBlock services courseId lastIndex

| EditingBlock (courseId, blockId, index, title, _) ->
  dispatcher.askEditingBlock ()
  |> updateEditingBlock services courseId blockId index title

| ListingBlocks (courseId, page, count, _) ->
  dispatcher.askListingBlocks ()
  |> updateListingBlocks services courseId page count
