module InsightClub.Creator.Bot.Bot


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
  { askInactive: Port<Inactive.Command>
    askIdle: Port<Idle.Command>
    askCreatingCourse: Port<CreatingCourse.Command>
    askEditingCourse: Port<EditingCourse.Command>
    askEditingTitle: Port<EditingTitle.Command>
    askEditingDesc: Port<EditingDesc.Command<'Effect>>
    askListingCourses: Port<ListingCourses.Command<'Effect>>
    askCreatingBlock: Port<CreatingBlock.Command>
    askEditingBlock: Port<EditingBlock.Command<'Effect>>
    askListingBlocks: Port<ListingBlocks.Command<'Effect>> }

type Service<'Param, 'Result> =
  ('Param -> 'Result) -> 'Result

type Service<'Result> =
  Service<unit, 'Result>

type Services<'Result> =
  { tryCreateCourse:
      CourseTitle -> Service<Result<CourseId, TitleError>, 'Result>

    tryUpdateTitle:
      CourseId -> CourseTitle -> Service<Result<unit, TitleError>, 'Result>

    tryUpdateDesc:
      CourseId -> CourseDesc -> Service<bool, 'Result>

    checkAnyCourses:
      Service<bool, 'Result>

    getCoursesCount:
      Service<Count, 'Result>

    tryCreateBlock:
      CourseId -> Index -> BlockTitle -> Service<BlockId option, 'Result>

    addContent:
      BlockId -> Content -> Service<'Result>

    getBlockInfo:
      BlockId -> Service<Index * BlockTitle, 'Result>

    getBlocksCount:
      CourseId -> Service<Count, 'Result>

    checkAnyBlocks:
      CourseId -> Service<bool, 'Result>

    getBlockContents:
      BlockId -> Service<Content list, 'Result>

    getBlockInfoByIndex:
      CourseId -> Index -> Service<BlockId * BlockTitle, 'Result>

    cleanBlock:
      BlockId -> Service<bool, 'Result> }

type Return<'Effect, 'Result> =
  State -> 'Effect option -> 'Result

// Values
/// Initial state
let initialState = Inactive

let private coursesPerPage = 5

let private onlyState state (return': Return<'Effect, 'Result>) =
  return' state None

let private withEffect (state, effect) (return': Return<'Effect, 'Result>) =
  return' state (Some effect)

let private updateInactive = function
| Some Inactive.Start ->
  Idle Idle.Started

| None ->
  Inactive

let private updateIdle return' services = function
| Some Idle.Help ->
  return' |> onlyState (Idle Idle.Helping)

| Some Idle.CreateCourse ->
  return' |> onlyState (CreatingCourse CreatingCourse.Started)

| Some Idle.EditCourse ->
  services.checkAnyCourses <|
    fun any ->
      let newState =
        if any then
          ListingCourses
            { Page = 0
              Count = coursesPerPage
              Msg = ListingCourses.Started }
        else
          Idle Idle.NoCourses

      return' |> onlyState newState

| None ->
  return' |> onlyState (Idle Idle.Error)

let private updateCreatingCourse return' services = function
| Some CreatingCourse.Cancel ->
  return' |> onlyState (Idle Idle.CreateCanceled)

| Some (CreatingCourse.CreateCourse title) ->
  services.tryCreateCourse title <|
    fun res ->
      let newState =
        match res with
        | Ok courseId ->
          EditingCourse (courseId, EditingCourse.CourseCreated)

        | Error error ->
          CreatingCourse (CreatingCourse.TitleError error)

      return' |> onlyState newState

| None ->
  return' |> onlyState (CreatingCourse CreatingCourse.Error)

let private updateEditingCourse return' services courseId = function
| Some EditingCourse.EditTitle ->
    let newState =
      EditingTitle (courseId, EditingTitle.Started)

    return' |> onlyState newState

| Some EditingCourse.EditDesc ->
  let newState =
    EditingDesc (courseId, EditingDesc.Started)

  return' |> onlyState newState

| Some EditingCourse.Exit ->
  return' |> onlyState (Idle Idle.ExitedEditing)

| Some EditingCourse.AddBlock ->
  services.getBlocksCount courseId <|
    fun count ->
      let newState =
        CreatingBlock
          { CourseId = courseId
            Index = count
            Msg = CreatingBlock.Started }

      return' |> onlyState newState

| Some EditingCourse.EditBlock ->
  services.checkAnyBlocks courseId <|
    fun any ->
      let newState =
        if any then
          ListingBlocks
            { CourseId = courseId
              Page = 0
              Count = coursesPerPage
              Msg = ListingBlocks.Started }
        else
          EditingCourse (courseId, EditingCourse.NoBlocks)

      return' |> onlyState newState

| None ->
  let newState =
    EditingCourse (courseId, EditingCourse.Error)

  return' |> onlyState newState

let private updateEditingTitle return' services courseId = function
| Some EditingTitle.Cancel ->
  let newState =
    EditingCourse (courseId, EditingCourse.TitleCanceled)

  return' |> onlyState newState

| Some (EditingTitle.SetTitle title) ->
  services.tryUpdateTitle courseId title <|
    fun res ->
      let newState =
        match res with
        | Ok () ->
          EditingCourse (courseId, EditingCourse.TitleSet)

        | Error error ->
          EditingTitle (courseId, EditingTitle.TitleError error)

      return' |> onlyState newState

| None ->
  let newState =
     EditingTitle (courseId, EditingTitle.Error)

  return' |> onlyState newState

let private updateEditingDesc return' services courseId = function
| Some EditingDesc.Cancel ->
  let newState =
    EditingCourse (courseId, EditingCourse.DescCanceled)

  return' |> onlyState newState

| Some (EditingDesc.SetDesc desc) ->
  services.tryUpdateDesc courseId desc <|
    fun success ->
      let newState =
        if success then
          EditingCourse (courseId, EditingCourse.DescSet)
        else
          EditingDesc (courseId, EditingDesc.DescTooLong)

      return' |> onlyState newState

| None ->
  let newState =
    EditingDesc (courseId, EditingDesc.Error)

  return' |> onlyState newState

let private updateListingCourses
  return' services (subState: ListingCourses.State) = function
| Some (ListingCourses.Select courseId) ->
  let newState =
    EditingCourse (courseId, EditingCourse.Editing)

  return' |> onlyState newState

| Some (ListingCourses.Prev beginningReached) ->
  if subState.Page = 0 then
    let newState =
      ListingCourses
        { subState with
            Msg = ListingCourses.Started }

    return' |> withEffect (newState, beginningReached)
  else
    let newState =
      ListingCourses
        { subState with
            Page = subState.Page - 1
            Msg = ListingCourses.Started }

    return' |> onlyState newState

| Some (ListingCourses.Next endingReached) ->
  services.getCoursesCount <|
    fun coursesCount ->
      if (subState.Page + 1) * subState.Count >= coursesCount then
        let newState =
          ListingCourses
            { subState with
                Msg = ListingCourses.Started }

        return' |> withEffect (newState, endingReached)
      else
        let newState =
          ListingCourses
            { subState with
                Page = subState.Page + 1
                Msg = ListingCourses.Started }

        return' |> onlyState newState

| Some ListingCourses.Exit ->
  return' |> onlyState (Idle Idle.EditCanceled)

| None ->
  let newState =
    ListingCourses
      { subState with
          Msg = ListingCourses.Error }

  return' |> onlyState newState

let private updateCreatingBlock
  return' services (subState: CreatingBlock.State) = function
| Some CreatingBlock.Cancel ->
  let newState =
    EditingCourse (subState.CourseId, EditingCourse.NewBlockCanceled)

  return' |> onlyState newState

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

      return' |> onlyState newState

| None ->
  let newState =
    CreatingBlock
      { subState with
          Msg = CreatingBlock.Error }

  return' |> onlyState newState

let private updateEditingBlock return' services (subState: EditingBlock.State)
  = function
| Some EditingBlock.Back ->
  let newState =
    EditingCourse (subState.CourseId, EditingCourse.Editing)

  return' |> onlyState newState

| Some EditingBlock.Nothing ->
  return' |> onlyState (EditingBlock subState)

| Some EditingBlock.InsertBefore ->
  let newState =
    CreatingBlock
      { CourseId = subState.CourseId
        Index = subState.Index
        Msg = CreatingBlock.Started }

  return' |> onlyState newState

| Some EditingBlock.InsertAfter ->
  let newState =
    CreatingBlock
      { CourseId = subState.CourseId
        Index = subState.Index + 1
        Msg = CreatingBlock.Started }

  return' |> onlyState newState

| Some (EditingBlock.Prev beginningReached) ->
  if subState.Index = 0 then
    let newState =
      EditingBlock
        { subState with
            Msg = EditingBlock.Started }

    return' |> withEffect (newState, beginningReached)
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

        return' |> onlyState newState

| Some (EditingBlock.Next endingReached) ->
  services.getBlocksCount subState.CourseId <|
    fun count ->
      if subState.Index = count - 1 then
        let newState =
          EditingBlock
            { subState with
                Msg = EditingBlock.Started }

        return' |> withEffect (newState, endingReached)
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

            return' |> onlyState newState

| Some (EditingBlock.Show showContents) ->
  services.getBlockContents subState.BlockId <|
    fun contents ->
      let newState =
        EditingBlock <|
          { subState with
              Msg = EditingBlock.Started }

      return' |> withEffect (newState, showContents contents)

| Some (EditingBlock.Clean blockEmpty) ->
  services.cleanBlock subState.BlockId <|
    fun cleaned ->
      let newState =
        EditingBlock
          { subState with
              Msg = EditingBlock.Cleaned }

      if cleaned then
        return' |> onlyState newState
      else
        return' |> withEffect (newState, blockEmpty)

| Some (EditingBlock.AddContent content) ->
  services.addContent subState.BlockId content <|
    fun () ->
      let newState =
        EditingBlock
          { subState with
              Msg = EditingBlock.ContentAdded content }

      return' |> onlyState newState

| None ->
  let newState =
    EditingBlock
      { subState with
          Msg = EditingBlock.Error }

  return' |> onlyState newState

let updateListingBlocks
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

      return' |> onlyState newState

| Some (ListingBlocks.Prev beginningReached) ->
  if subState.Page = 0 then
    let newState =
      ListingBlocks
        { subState with
            Msg = ListingBlocks.Started }

    return' |> withEffect (newState, beginningReached)
  else
    let newState =
      ListingBlocks
        { subState with
            Page = subState.Page - 1
            Msg = ListingBlocks.Started }

    return' |> onlyState newState

| Some (ListingBlocks.Next endingReached) ->
  services.getBlocksCount subState.CourseId <|
    fun blocksCount ->
      if (subState.Page + 1) * subState.Count >= blocksCount then
        let newState =
          ListingBlocks
            { subState with
                Msg = ListingBlocks.Started }

        return' |> withEffect (newState, endingReached)
      else
        let newState =
          ListingBlocks
            { subState with
                Page = subState.Page + 1
                Msg = ListingBlocks.Started }

        return' |> onlyState newState

| Some ListingBlocks.Back ->
  let newState =
    EditingCourse (subState.CourseId, EditingCourse.BlockCanceled)

  return' |> onlyState newState

| None ->
  let newState =
    ListingBlocks
      { subState with
          Msg = ListingBlocks.Error }

  return' |> onlyState newState

let update return' dispatcher services = function
| Inactive ->
  dispatcher.askInactive ()
  |> updateInactive
  |> onlyState <| return'

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
