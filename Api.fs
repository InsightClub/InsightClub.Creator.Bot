module InsightClub.Creator.Bot.Api

open Core
open System
open Funogram
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types


// Commands
module Command =
  let start = "/start"
  let new' = "/new"
  let cancel = "/cancel"
  let exit = "/exit"
  let edit = "/edit"

let getCommands ctx =
  let getInactive () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) Command.start)
    |> Option.map (always Inactive.Start)

  let getIdle () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) Command.new')
    |> Option.map (always Idle.CreateCourse)

  let getCreatingCourse () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command Command.cancel) } } ->
      Some CreatingCourse.Cancel

    | { Message = Some { Text = Some (PlainText courseTitle) } } ->
      Some <| CreatingCourse.CreateCourse courseTitle

    | _ ->
      None

  let getEditingCourse () =
    match ctx.Update.CallbackQuery with
    | Some { Data = Some (Command Command.edit) } ->
      Some EditingCourse.EditTitle

    | Some { Data = Some (Command Command.exit) } ->
      Some EditingCourse.Exit

    | _ ->
      None

  let getEditingTitle () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command Command.cancel) } } ->
      Some EditingTitle.Cancel

    | { Message = Some { Text = Some (PlainText courseTitle) } } ->
      Some <| EditingTitle.SetTitle courseTitle

    | _ ->
      None

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse
    getEditingTitle = getEditingTitle }

// Api helpers
let getUser ctx =
  match ctx.Update with
  | { Message = Some { From = Some user } } ->
    Some user

  | { CallbackQuery = Some { From = user } } ->
    Some user

  | _ ->
    None

let button text data =
  { Text = text
    CallbackData = Some data
    Url = None
    Pay = None
    LoginUrl = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None }

let inlineMarkup =
  Option.map
    ( fun markup ->
      { InlineKeyboard = List.map Seq.ofList markup } )

let markup =
  inlineMarkup
  >> Option.map InlineKeyboardMarkup

let removeLastMarkupMaybe config lastId userId =
  async
    { if Option.isSome lastId then
        do!
          Api.editMessageReplyMarkupBase
            (Some <| Int userId) lastId None None
          |> Api.api config
          |> Async.Ignore }

let sendMessage config lastId userId text keyboard  =
  if not <| String.IsNullOrEmpty text then
    Api.sendMessageBase
      (Int userId) text None None None None (markup keyboard)
    |> Api.api config
    |> Async.map
      ( fun r ->
          match r with
          | Ok m ->
            m.ReplyMarkup
            |> Option.map (always m.MessageId)

          | Error _ -> None )
  else
    Async.singleton lastId

let answerCallbackQuery config (query: CallbackQuery) =
  Api.answerCallbackQueryBase
    (Some query.Id) (Some String.Empty) None None None
  |> Api.api config
  |> Async.Ignore

let editMessage config lastId userId text keyboard =
  let id = Some <| Int userId

  if not <| String.IsNullOrEmpty text then
    Api.editMessageTextBase
      id lastId None text None None (inlineMarkup keyboard)
    |> Api.api config
    |> Async.Ignore
  else
    Async.doNothing

let idleMessage (user: User) =
  function
  | Idle.Started ->
    Message.greeting user.FirstName user.LastName

  | Idle.CourseCanceled ->
    Message.courseCanceled

  | Idle.ExitedEditing ->
    Message.exitedEditing

  | Idle.Error ->
    Message.error

let creatingCourseMessage =
  function
  | CreatingCourse.Started ->
    Message.courseStarted

  | CreatingCourse.TitleReserved ->
    Message.titleReserved

  | CreatingCourse.Error ->
    Message.error

let editingCourseMessage =
  function
  | EditingCourse.Started ->
    Message.editingCourse

  | EditingCourse.TitleCanceled ->
    Message.titleCanceled

  | EditingCourse.TitleSet ->
    Message.titleSet

  | EditingCourse.Error ->
    Message.error

let editingTitleMessage =
  function
  | EditingTitle.Started ->
    Message.editingTitle

  | EditingTitle.TitleReserved ->
    Message.titleReserved

  | EditingTitle.Error ->
    Message.error

// Response
let respond (ctx: UpdateContext) lastId state =
  // updateArrived must ensure user is present, so this call is safe
  let user = getUser ctx |> Option.get

  let message, keyboard =
    match state with
    | Inactive ->
      String.Empty, None

    | Idle data ->
      idleMessage user data, None

    | CreatingCourse data ->
      creatingCourseMessage data,
      Some [ [ button Message.cancel Command.cancel ] ]

    | EditingCourse (_, data) ->
      editingCourseMessage data,
      Some
        [ [ button Message.editTitle Command.edit ]
          [ button Message.exit Command.exit ] ]

    | EditingTitle (_, data) ->
      editingTitleMessage data,
      Some [ [ button Message.cancel Command.cancel ] ]

  match ctx.Update with
  | { Message = Some _ } ->
    async
      { let! _ =
          removeLastMarkupMaybe ctx.Config lastId user.Id
          |> Async.StartChild

        return!
          sendMessage ctx.Config lastId user.Id message keyboard }

  | { CallbackQuery = Some query } ->
    async
      { let! _ =
          answerCallbackQuery ctx.Config query
          |> Async.StartChild

        let! _ =
          editMessage ctx.Config lastId user.Id message keyboard
          |> Async.StartChild

        return
          keyboard
          |> Option.bind (always lastId) }

  | _ ->
    Async.singleton lastId

// Main function
let updateArrived getConnection ctx =
  ctx
  |> getUser // Ensure user is present
  |> Option.map
      ( fun user ->
          async
            { use connection = getConnection ()
              let! creatorId, lastId, botState = State.get connection user.Id
              let services = Services.get connection creatorId
              let commands = getCommands ctx
              let callback = Async.singleton
              let! newBotState = update services commands callback botState
              let! newLastId = respond ctx lastId newBotState
              let state = State.create newLastId newBotState
              do! State.update connection creatorId state } )

  |> Option.defaultValue Async.doNothing
