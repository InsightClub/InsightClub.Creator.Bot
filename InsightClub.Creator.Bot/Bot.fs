module InsightClub.Creator.Bot.Bot


// ############################################
// ###                TYPES                 ###
// ############################################

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
    | EditCourse

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
    | EditBlock
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

type Command = Ignore

type State =
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

type Port<'Command> = unit -> 'Command option

type Dispatcher<'Effect> =
  { askGlobal: Port<Command>
    askInactive: Port<Inactive.Command>
    askIdle: Port<Idle.Command>
    askCreatingCourse: Port<CreatingCourse.Command>
    askEditingCourse: Port<EditingCourse.Command>
    askEditingTitle: Port<EditingTitle.Command>
    askEditingDesc: Port<EditingDesc.Command<'Effect>>
    askListingCourses: Port<ListingCourses.Command<'Effect>>
    askCreatingBlock: Port<CreatingBlock.Command>
    askEditingBlock: Port<EditingBlock.Command<'Effect>>
    askListingBlocks: Port<ListingBlocks.Command<'Effect>> }

type Apply<'Param, 'Result> =
  ('Param -> 'Result) -> 'Result

type Apply<'Result> =
  Apply<unit, 'Result>

type Services<'Result> =
  { tryCreateCourse:
      CourseTitle -> Apply<Result<CourseId, TitleError>, 'Result>

    tryUpdateTitle:
      CourseId -> CourseTitle -> Apply<Result<unit, TitleError>, 'Result>

    tryUpdateDesc:
      CourseId -> CourseDesc -> Apply<bool, 'Result>

    checkAnyCourses:
      Apply<bool, 'Result>

    getCoursesCount:
      Apply<Count, 'Result>

    tryCreateBlock:
      CourseId -> Index -> BlockTitle -> Apply<BlockId option, 'Result>

    addContent:
      BlockId -> Content -> Apply<'Result>

    getBlockInfo:
      BlockId -> Apply<Index * BlockTitle, 'Result>

    getBlocksCount:
      CourseId -> Apply<Count, 'Result>

    checkAnyBlocks:
      CourseId -> Apply<bool, 'Result>

    getBlockContents:
      BlockId -> Apply<Content list, 'Result>

    getBlockInfoByIndex:
      CourseId -> Index -> Apply<BlockId * BlockTitle, 'Result>

    cleanBlock:
      BlockId -> Apply<bool, 'Result> }

type private Return<'Effect, 'Result> =
  { withNoEffects: State -> 'Result
    withEffect: State -> 'Effect -> 'Result }


// ############################################
// ###                VALUES                ###
// ############################################

let initialState = Inactive

let private coursesPerPage = 5

let private updateInactive = function
| Some Inactive.Start ->
  Idle Idle.Started

| None ->
  Inactive

let private updateIdle return' services = function
| Some Idle.Help ->
  return'.withNoEffects (Idle Idle.Helping)

| Some Idle.CreateCourse ->
  return'.withNoEffects (CreatingCourse CreatingCourse.Started)

| Some Idle.EditCourse ->
  services.checkAnyCourses <|
    fun isAny ->
      let newState =
        if isAny then
          ListingCourses
            { Page = 0
              Count = coursesPerPage
              Msg = ListingCourses.Started }
        else
          Idle Idle.NoCourses

      return'.withNoEffects newState

| None ->
  return'.withNoEffects (Idle Idle.Error)

let private updateCreatingCourse return' services = function
| Some CreatingCourse.Cancel ->
  return'.withNoEffects (Idle Idle.CreateCanceled)

| Some (CreatingCourse.CreateCourse title) ->
  services.tryCreateCourse title <|
    fun result ->
      let newState =
        match result with
        | Ok courseId ->
          EditingCourse (courseId, EditingCourse.CourseCreated)

        | Error error ->
          CreatingCourse (CreatingCourse.TitleError error)

      return'.withNoEffects newState

| None ->
  return'.withNoEffects (CreatingCourse CreatingCourse.Error)

let private updateEditingCourse return' services courseId = function
| Some EditingCourse.EditTitle ->
    let newState =
      EditingTitle (courseId, EditingTitle.Started)

    return'.withNoEffects newState

| Some EditingCourse.EditDesc ->
  let newState =
    EditingDesc (courseId, EditingDesc.Started)

  return'.withNoEffects newState

| Some EditingCourse.Exit ->
  return'.withNoEffects (Idle Idle.ExitedEditing)

| Some EditingCourse.AddBlock ->
  services.getBlocksCount courseId <|
    fun count ->
      let newState =
        CreatingBlock
          { CourseId = courseId
            Index = count
            Msg = CreatingBlock.Started }

      return'.withNoEffects newState

| Some EditingCourse.EditBlock ->
  services.checkAnyBlocks courseId <|
    fun isAny ->
      let newState =
        if isAny then
          ListingBlocks
            { CourseId = courseId
              Page = 0
              Count = coursesPerPage
              Msg = ListingBlocks.Started }
        else
          EditingCourse (courseId, EditingCourse.NoBlocks)

      return'.withNoEffects newState

| None ->
  let newState =
    EditingCourse (courseId, EditingCourse.Error)

  return'.withNoEffects newState

let private updateEditingTitle return' services courseId = function
| Some EditingTitle.Cancel ->
  let newState =
    EditingCourse (courseId, EditingCourse.TitleCanceled)

  return'.withNoEffects newState

| Some (EditingTitle.SetTitle title) ->
  services.tryUpdateTitle courseId title <|
    fun result ->
      let newState =
        match result with
        | Ok () ->
          EditingCourse (courseId, EditingCourse.TitleSet)

        | Error error ->
          EditingTitle (courseId, EditingTitle.TitleError error)

      return'.withNoEffects newState

| None ->
  let newState =
     EditingTitle (courseId, EditingTitle.Error)

  return'.withNoEffects newState

let private updateEditingDesc return' services courseId = function
| Some EditingDesc.Cancel ->
  let newState =
    EditingCourse (courseId, EditingCourse.DescCanceled)

  return'.withNoEffects newState

| Some (EditingDesc.SetDesc desc) ->
  services.tryUpdateDesc courseId desc <|
    fun isUpdated ->
      let newState =
        if isUpdated then
          EditingCourse (courseId, EditingCourse.DescSet)
        else
          EditingDesc (courseId, EditingDesc.DescTooLong)

      return'.withNoEffects newState

| None ->
  let newState =
    EditingDesc (courseId, EditingDesc.Error)

  return'.withNoEffects newState

let private updateListingCourses
  return' services (subState: ListingCourses.State) = function
| Some (ListingCourses.Select courseId) ->
  let newState =
    EditingCourse (courseId, EditingCourse.Editing)

  return'.withNoEffects newState

| Some (ListingCourses.Prev beginningReached) ->
  if subState.Page = 0 then
    let newState =
      ListingCourses
        { subState with
            Msg = ListingCourses.Started }

    return'.withEffect newState beginningReached
  else
    let newState =
      ListingCourses
        { subState with
            Page = subState.Page - 1
            Msg = ListingCourses.Started }

    return'.withNoEffects newState

| Some (ListingCourses.Next endingReached) ->
  services.getCoursesCount <|
    fun coursesCount ->
      if (subState.Page + 1) * subState.Count >= coursesCount then
        let newState =
          ListingCourses
            { subState with
                Msg = ListingCourses.Started }

        return'.withEffect newState endingReached
      else
        let newState =
          ListingCourses
            { subState with
                Page = subState.Page + 1
                Msg = ListingCourses.Started }

        return'.withNoEffects newState

| Some ListingCourses.Exit ->
  return'.withNoEffects (Idle Idle.EditCanceled)

| None ->
  let newState =
    ListingCourses
      { subState with
          Msg = ListingCourses.Error }

  return'.withNoEffects newState

let private updateCreatingBlock
  return' services (subState: CreatingBlock.State) = function
| Some CreatingBlock.Cancel ->
  let newState =
    EditingCourse (subState.CourseId, EditingCourse.NewBlockCanceled)

  return'.withNoEffects newState

| Some (CreatingBlock.CreateBlock title) ->
  services.tryCreateBlock subState.CourseId subState.Index title <|
    fun blockId ->
      let newState =
        match blockId with
        | Some blockId ->
          EditingBlock
            { CourseId = subState.CourseId
              BlockId = blockId
              Index = subState.Index
              Title = title
              Msg = EditingBlock.Started }

        | None ->
          CreatingBlock
            { subState with
                Msg = CreatingBlock.TitleReserved }

      return'.withNoEffects newState

| None ->
  let newState =
    CreatingBlock
      { subState with
          Msg = CreatingBlock.Error }

  return'.withNoEffects newState

let private updateEditingBlock return' services (subState: EditingBlock.State)
  = function
| Some EditingBlock.Back ->
  let newState =
    EditingCourse (subState.CourseId, EditingCourse.Editing)

  return'.withNoEffects newState

| Some EditingBlock.InsertBefore ->
  let newState =
    CreatingBlock
      { CourseId = subState.CourseId
        Index = subState.Index
        Msg = CreatingBlock.Started }

  return'.withNoEffects newState

| Some EditingBlock.InsertAfter ->
  let newState =
    CreatingBlock
      { CourseId = subState.CourseId
        Index = subState.Index + 1
        Msg = CreatingBlock.Started }

  return'.withNoEffects newState

| Some (EditingBlock.Prev beginningReached) ->
  if subState.Index = 0 then
    let newState =
      EditingBlock
        { subState with
            Msg = EditingBlock.Started }

    return'.withEffect newState beginningReached
  else
    services.getBlockInfoByIndex subState.CourseId (subState.Index - 1) <|
      fun (blockId, title) ->
        let newState =
          EditingBlock
            { subState with
                BlockId = blockId
                Index = subState.Index - 1
                Title = title
                Msg = EditingBlock.Started }

        return'.withNoEffects newState

| Some (EditingBlock.Next endingReached) ->
  services.getBlocksCount subState.CourseId <|
    fun count ->
      if subState.Index = count - 1 then
        let newState =
          EditingBlock
            { subState with
                Msg = EditingBlock.Started }

        return'.withEffect newState endingReached
      else
        services.getBlockInfoByIndex subState.CourseId (subState.Index + 1) <|
          fun (blockId, title) ->
            let newState =
              EditingBlock <|
              { subState with
                  BlockId = blockId
                  Index = subState.Index + 1
                  Title = title
                  Msg = EditingBlock.Started }

            return'.withNoEffects newState

| Some (EditingBlock.Show showContents) ->
  services.getBlockContents subState.BlockId <|
    fun contents ->
      let newState =
        EditingBlock <|
          { subState with
              Msg = EditingBlock.Started }

      return'.withEffect newState (showContents contents)

| Some (EditingBlock.Clean blockEmpty) ->
  services.cleanBlock subState.BlockId <|
    fun cleaned ->
      let newState =
        EditingBlock
          { subState with
              Msg = EditingBlock.Cleaned }

      if cleaned then
        return'.withNoEffects newState
      else
        return'.withEffect newState blockEmpty

| Some (EditingBlock.AddContent content) ->
  services.addContent subState.BlockId content <|
    fun () ->
      let newState =
        EditingBlock
          { subState with
              Msg = EditingBlock.ContentAdded content }

      return'.withNoEffects newState

| None ->
  let newState =
    EditingBlock
      { subState with
          Msg = EditingBlock.Error }

  return'.withNoEffects newState

let private updateListingBlocks
  return' services (subState: ListingBlocks.State) = function
| Some (ListingBlocks.Select blockId) ->
  services.getBlockInfo blockId <|
    fun (index, title) ->
      let newState =
        EditingBlock
          { CourseId = subState.CourseId
            BlockId = blockId
            Index = index
            Title = title
            Msg = EditingBlock.Started }

      return'.withNoEffects newState

| Some (ListingBlocks.Prev beginningReached) ->
  if subState.Page = 0 then
    let newState =
      ListingBlocks
        { subState with
            Msg = ListingBlocks.Started }

    return'.withEffect newState beginningReached
  else
    let newState =
      ListingBlocks
        { subState with
            Page = subState.Page - 1
            Msg = ListingBlocks.Started }

    return'.withNoEffects newState

| Some (ListingBlocks.Next endingReached) ->
  services.getBlocksCount subState.CourseId <|
    fun blocksCount ->
      if (subState.Page + 1) * subState.Count >= blocksCount then
        let newState =
          ListingBlocks
            { subState with
                Msg = ListingBlocks.Started }

        return'.withEffect newState endingReached
      else
        let newState =
          ListingBlocks
            { subState with
                Page = subState.Page + 1
                Msg = ListingBlocks.Started }

        return'.withNoEffects newState

| Some ListingBlocks.Back ->
  let newState =
    EditingCourse (subState.CourseId, EditingCourse.BlockCanceled)

  return'.withNoEffects newState

| None ->
  let newState =
    ListingBlocks
      { subState with
          Msg = ListingBlocks.Error }

  return'.withNoEffects newState

let private updateLocal return' dispatcher services = function
| Inactive ->
  let newState =
    dispatcher.askInactive ()
    |> updateInactive

  return'.withNoEffects newState

| Idle _ ->
  dispatcher.askIdle ()
  |> updateIdle return' services

| CreatingCourse _ ->
  dispatcher.askCreatingCourse ()
  |> updateCreatingCourse return' services

| EditingCourse (courseId, _) ->
  dispatcher.askEditingCourse ()
  |> updateEditingCourse return' services courseId

| EditingTitle (courseId, _) ->
  dispatcher.askEditingTitle ()
  |> updateEditingTitle return' services courseId

| EditingDesc (courseId, _) ->
  dispatcher.askEditingDesc ()
  |> updateEditingDesc return' services courseId

| ListingCourses subState ->
  dispatcher.askListingCourses ()
  |> updateListingCourses return' services subState

| CreatingBlock subState ->
  dispatcher.askCreatingBlock ()
  |> updateCreatingBlock return' services subState

| EditingBlock subState ->
  dispatcher.askEditingBlock ()
  |> updateEditingBlock return' services subState

| ListingBlocks subState ->
  dispatcher.askListingBlocks ()
  |> updateListingBlocks return' services subState

let update return' dispatcher services state =
  let return' =
    { withNoEffects = fun state -> return' state None
      withEffect = fun state effect -> return' state (Some effect) }

  match dispatcher.askGlobal () with
  | Some Ignore ->
    return'.withNoEffects state

  | None ->
    updateLocal return' dispatcher services state
