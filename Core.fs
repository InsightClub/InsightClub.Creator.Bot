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
    | Prev of beginningReached: 'Effect
    | Next of endingReached: 'Effect
    | Exit

  type Msg =
    | Started
    | Error

  type State =
    { Page: Page
      Count: Count
      Msg: Msg }

module CreatingBlock =
  type Command =
    | Cancel
    | CreateBlock of BlockTitle

  type Msg =
    | Started
    | TitleReserved
    | Error

  type State =
    { CourseId: CourseId
      Index: Index
      Msg: Msg }

module EditingBlock =
  type Command<'Effect> =
    | Back
    | Nothing
    | InsertBefore
    | InsertAfter
    | Prev of beginningReached: 'Effect
    | Next of endingReached: 'Effect
    | Show of showContent: (Content list -> 'Effect)
    | Clean of blockEmpty: 'Effect
    | AddContent of Content

  type Msg =
    | Started
    | ContentAdded of Content
    | Cleaned
    | Error

  type State =
    { CourseId: CourseId
      BlockId: BlockId
      Index: Index
      Title: BlockTitle
      Msg: Msg }

module ListingBlocks =
  type Command<'Effect> =
    | Select of BlockId
    | Prev of beginningReached: 'Effect
    | Next of endingReached: 'Effect
    | Back

  type Msg =
    | Started
    | Error

  type State =
    { CourseId: CourseId
      Page: Page
      Count: Count
      Msg: Msg }

type BotState =
  | Inactive
  | Idle of Idle.Msg
  | CreatingCourse of CreatingCourse.Msg
  | EditingCourse of CourseId * EditingCourse.Msg
  | EditingTitle of CourseId * EditingTitle.Msg
  | EditingDesc of CourseId * EditingDesc.Msg
  | ListingCourses of ListingCourses.State
  | CreatingBlock of CreatingBlock.State
  | EditingBlock of EditingBlock.State
  | ListingBlocks of ListingBlocks.State

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
      let newState =
        if any then
          let newSubState : ListingCourses.State =
            { Page = 0
              Count = count
              Msg = ListingCourses.Started}

          ListingCourses newSubState
        else
          Idle Idle.NoCourses

      services.callback newState None

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
      let newSubState : CreatingBlock.State =
        { CourseId = courseId
          Index = count
          Msg = CreatingBlock.Started }

      services.callback (CreatingBlock newSubState) None

| Some (EditingCourse.EditBlock count) ->
  services.checkAnyBlocks courseId <|
    fun any ->
      let state =
        if any then
          let newSubState : ListingBlocks.State =
            { CourseId = courseId
              Page = 0
              Count = count
              Msg = ListingBlocks.Started }

          ListingBlocks newSubState
        else
          let newSubState =
            courseId, EditingCourse.NoBlocks

          EditingCourse newSubState

      services.callback state None

| None ->
  services.callback (EditingCourse (courseId, EditingCourse.Error)) None

let private updateEditingTitle services courseId = function
| Some EditingTitle.Cancel ->
  let newSubState =
    courseId, EditingCourse.TitleCanceled

  services.callback (EditingCourse newSubState) None

| Some (EditingTitle.SetTitle title) ->
  services.tryUpdateTitle courseId title <|
    function
    | Ok () ->
      let newSubState =
        courseId, EditingCourse.TitleSet

      services.callback (EditingCourse newSubState) None

    | Error error ->
      let newSubState =
         courseId, EditingTitle.TitleError error

      services.callback (EditingTitle newSubState) None

| None ->
  let newSubState =
     courseId, EditingTitle.Error

  services.callback (EditingTitle newSubState) None

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

let private updateListingCourses services (subState: ListingCourses.State)
  = function
| Some (ListingCourses.Select courseId) ->
  services.callback (EditingCourse (courseId, EditingCourse.Editing)) None

| Some (ListingCourses.Prev beginningReached) ->
  let newState, effect =
    if subState.Page = 0 then
      let newSubState =
        { subState with
            Msg = ListingCourses.Started }

      ListingCourses newSubState, Some beginningReached
    else
      let newSubState =
        { subState with
            Page = subState.Page - 1
            Msg = ListingCourses.Started }

      ListingCourses newSubState, None

  services.callback newState effect

| Some (ListingCourses.Next endingReached) ->
  services.getCoursesCount <|
    fun coursesCount ->
      let newState, effect =
        if (subState.Page + 1) * subState.Count >= coursesCount then
          let newSubState =
            { subState with
                Msg = ListingCourses.Started }

          ListingCourses newSubState, Some endingReached
        else
          let newSubState =
            { subState with
                Page = subState.Page + 1
                Msg = ListingCourses.Started }

          ListingCourses newSubState, None

      services.callback newState effect

| Some ListingCourses.Exit ->
  services.callback (Idle Idle.EditCanceled) None

| None ->
  let newSubState =
    { subState with
        Msg = ListingCourses.Error }

  services.callback (ListingCourses newSubState) None

let private updateCreatingBlock services (subState: CreatingBlock.State)
  = function
| Some CreatingBlock.Cancel ->
  let newSubState =
    subState.CourseId, EditingCourse.NewBlockCanceled

  services.callback (EditingCourse newSubState) None

| Some (CreatingBlock.CreateBlock title) ->
  services.tryCreateBlock subState.CourseId subState.Index title <|
    function
    | Some blockId ->
      let newSubState : EditingBlock.State =
        { CourseId = subState.CourseId
          BlockId = blockId
          Index = subState.Index
          Title = title
          Msg = EditingBlock.Started }

      services.callback (EditingBlock newSubState) None

    | None ->
      let newSubState =
        { subState with
            Msg = CreatingBlock.TitleReserved }

      services.callback (CreatingBlock newSubState) None

| None ->
  let newSubState =
    { subState with
        Msg = CreatingBlock.Error }

  services.callback (CreatingBlock newSubState) None

let private updateEditingBlock services (subState: EditingBlock.State)
  = function
| Some EditingBlock.Back ->
  let newSubState =
     subState.CourseId, EditingCourse.Editing

  services.callback (EditingCourse newSubState) None

| Some EditingBlock.Nothing ->
  services.callback (EditingBlock subState) None

| Some EditingBlock.InsertBefore ->
  let subState : CreatingBlock.State =
    { CourseId = subState.CourseId
      Index = subState.Index
      Msg = CreatingBlock.Started }

  services.callback (CreatingBlock subState) None

| Some EditingBlock.InsertAfter ->
  let subState : CreatingBlock.State =
    { CourseId = subState.CourseId
      Index = subState.Index + 1
      Msg = CreatingBlock.Started }

  services.callback (CreatingBlock subState) None

| Some (EditingBlock.Prev beginningReached) ->
  if subState.Index = 0 then
    let newSubState =
      { subState with
          Msg = EditingBlock.Started }

    services.callback (EditingBlock newSubState) (Some beginningReached)

  else
    services.getBlockInfoByIndex subState.CourseId (subState.Index - 1) <|
      fun (blockId, title) ->
        let newSubState =
          { subState with
              BlockId = blockId
              Index = subState.Index - 1
              Title = title
              Msg = EditingBlock.Started }

        services.callback (EditingBlock newSubState) None

| Some (EditingBlock.Next endingReached) ->
  services.getBlocksCount subState.CourseId <|
    fun count ->
      if subState.Index = count - 1 then
        let newSubState =
          { subState with
              Msg = EditingBlock.Started }

        services.callback (EditingBlock newSubState) (Some endingReached)
      else
        services.getBlockInfoByIndex subState.CourseId (subState.Index + 1) <|
          fun (blockId, title) ->
            let newSubState =
              { subState with
                  BlockId = blockId
                  Index = subState.Index + 1
                  Title = title
                  Msg = EditingBlock.Started }

            services.callback (EditingBlock newSubState) None

| Some (EditingBlock.Show showContents) ->
  services.getBlockContents subState.BlockId <|
    fun contents ->
      let newSubState =
        { subState with
            Msg = EditingBlock.Started }

      services.callback (EditingBlock newSubState) (Some <| showContents contents)

| Some (EditingBlock.Clean blockEmpty) ->
  services.cleanBlock subState.BlockId <|
    fun cleaned ->
      let newSubState =
        { subState with
            Msg = EditingBlock.Cleaned }

      let effect =
        if cleaned
        then None
        else Some blockEmpty

      services.callback (EditingBlock newSubState) effect

| Some (EditingBlock.AddContent content) ->
  services.addContent subState.BlockId content <|
    fun () ->
      let newSubState =
        { subState with
            Msg = EditingBlock.ContentAdded content }

      services.callback (EditingBlock newSubState) None

| None ->
  let newSubState =
    { subState with
        Msg = EditingBlock.Error }

  services.callback (EditingBlock newSubState) None

let updateListingBlocks services (subState: ListingBlocks.State) = function
| Some (ListingBlocks.Select blockId) ->
  services.getBlockInfo blockId <|
    fun (index, title) ->
      let newSubState : EditingBlock.State =
        { CourseId = subState.CourseId
          BlockId = blockId
          Index = index
          Title = title
          Msg = EditingBlock.Started }

      services.callback (EditingBlock newSubState) None

| Some (ListingBlocks.Prev beginningReached) ->
  let newState, effect =
    if subState.Page = 0 then
      let newSubState =
        { subState with
            Msg = ListingBlocks.Started }

      ListingBlocks newSubState, Some beginningReached
    else
      let newSubState =
        { subState with
            Page = subState.Page - 1
            Msg = ListingBlocks.Started }

      ListingBlocks newSubState, None

  services.callback newState effect

| Some (ListingBlocks.Next endingReached) ->
  services.getBlocksCount subState.CourseId <|
    fun blocksCount ->
      let newState, effect =
        if (subState.Page + 1) * subState.Count >= blocksCount then
          let newSubState =
            { subState with
                Msg = ListingBlocks.Started }

          ListingBlocks newSubState, Some endingReached
        else
          let newSubState =
            { subState with
                Page = subState.Page + 1
                Msg = ListingBlocks.Started }

          ListingBlocks newSubState, None

      services.callback newState effect

| Some ListingBlocks.Back ->
  let newSubState =
    subState.CourseId, EditingCourse.BlockCanceled

  services.callback (EditingCourse newSubState) None

| None ->
  let newSubState =
    { subState with
        Msg = ListingBlocks.Error }

  services.callback (ListingBlocks newSubState) None

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

| ListingCourses subState ->
  dispatcher.askListingCourses ()
  |> updateListingCourses services subState

| CreatingBlock subState ->
  dispatcher.askCreatingBlock ()
  |> updateCreatingBlock services subState

| EditingBlock subState ->
  dispatcher.askEditingBlock ()
  |> updateEditingBlock services subState

| ListingBlocks subState ->
  dispatcher.askListingBlocks ()
  |> updateListingBlocks services subState
