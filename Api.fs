module InsightClub.Creator.Bot.Api

open Core
open Funogram.Api
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open Microsoft.FSharpLu.Json


// Types
type TelegramBotState =
  /// Id of the last message sent with the inline keyboard
  { LastId: int64 option
    State: BotState }

// Helpers
module Json = Compact.Strict

let (|Command|_|) command s =
  if (s = command) then Some () else None

let (|PlainText|_|) (s: string) =
  if s.StartsWith "/" then None else Some s

let always x _ = x

// Services
let getServices connection creatorId =
  let tryCreateCourse courseTitle callback =
    async
      { let! courseIdOption =
          Repo.tryCreateCourse connection creatorId courseTitle

        return! callback courseIdOption }

  { tryCreateCourse = tryCreateCourse }

// Commands
let start = "/start"
let new' = "/new"
let cancelCourse = "/cancel_course"
let exitEditing = "/exit_editing"

let getCommands ctx =
  let getInactive () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) start)
    |> Option.map (always Inactive.Start)

  let getIdle () =
    ctx.Update.Message
    |> Option.bind (fun m -> m.Text)
    |> Option.filter ((=) new')
    |> Option.map (always Idle.CreateCourse)

  let getCreatingCourse () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command cancelCourse) } } ->
      Some CreatingCourse.Cancel

    | { Message = Some { Text = Some (PlainText courseTitle) } } ->
      Some <| CreatingCourse.CreateCourse courseTitle

    | _ ->
      None

  let getEditingCourse () =
    ctx.Update.CallbackQuery
    |> Option.bind (fun q -> q.Data)
    |> Option.filter ((=) exitEditing)
    |> Option.map (always EditingCourse.Exit)

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse }

// State
let initialStateJson =
  Json.serialize { LastId = None; State = initial }

let getState connection telegramId =
  Repo.getState initialStateJson connection telegramId
  |> Async.map
    ( fun (creatorId, stateJson) ->
        let { LastId = lastId; State = state } =
          Json.deserialize<TelegramBotState> stateJson

        creatorId, lastId, state )

let updateState connection creatorId (newState: TelegramBotState) =
  Repo.updateState connection creatorId (Json.serialize newState)

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

let inlineMarkup markup =
  { InlineKeyboard = List.map Seq.ofList markup }

let markup = inlineMarkup >> InlineKeyboardMarkup

// Response
let respond (ctx: UpdateContext) lastId =
  let user = Option.get <| getUser ctx
  let config = ctx.Config

  let sendMessage =
    Api.sendMessage user.Id
    >> api config

  let sendMessageMarkup text =
    Api.sendMessageMarkup user.Id text
    >> api config

  let editLastMessage text markup =
    Api.editMessageTextBase
      (Some <| Int user.Id) lastId None text None None markup
    |> api config

  let removeMarkup () =
    Api.editMessageReplyMarkupBase
      (Some <| Int user.Id) lastId None None
    |> api config
    |> Async.Ignore

  let answerCallback text =
    let Id = ctx.Update.CallbackQuery.Value.Id
    Api.answerCallbackQueryBase
      (Some Id) (Some text) None None None
    |> api config

  let andRemovePrevious comp =
    async
      { do! removeMarkup ()
        return! comp }

  let andAnswerCallbackEmpty comp =
    async
      { let! _ = answerCallback ""
        return! comp }

  let getMessageId comp =
    async
      { match! comp with
        | Ok { MessageId = id } ->
          return Some id

        | _ ->
          return lastId }

  function
  | Inactive ->
    Async.singleton None

  | Idle Idle.Started ->
    Message.greeting user.FirstName user.LastName
    |> sendMessage
    |> Async.always None

  | Idle Idle.CourseCanceled ->
    editLastMessage Message.courseCanceled None
    |> Async.always None
    |> andAnswerCallbackEmpty

  | Idle Idle.ExitedEditing ->
    editLastMessage Message.exitedEditing None
    |> Async.always None
    |> andAnswerCallbackEmpty

  | Idle Idle.Error ->
    Message.error
    |> sendMessage
    |> Async.always None

  | CreatingCourse CreatingCourse.Started ->
    [ [ button Message.cancel cancelCourse ] ]
    |> markup
    |> sendMessageMarkup Message.courseStarted
    |> getMessageId

  | CreatingCourse CreatingCourse.TitleReserved ->
    [ [ button Message.cancel cancelCourse ] ]
    |> markup
    |> sendMessageMarkup Message.titleReserved
    |> getMessageId
    |> andRemovePrevious

  | CreatingCourse CreatingCourse.Error ->
    [ [ button Message.cancel cancelCourse ] ]
    |> markup
    |> sendMessageMarkup Message.error
    |> getMessageId
    |> andRemovePrevious

  | EditingCourse _ ->
    [ [ button Message.exit exitEditing ] ]
    |> markup
    |> sendMessageMarkup Message.editingCourse
    |> getMessageId
    |> andRemovePrevious

// Main function
let updateArrived getConnection ctx =
  match getUser ctx with
  | Some user ->
    async
      { use connection = getConnection ()
        let! creatorId, lastId, state = getState connection user.Id
        let services = getServices connection creatorId
        let commands = getCommands ctx
        let callback = Async.singleton
        let! newState = update services commands callback state
        let! newLastId = respond ctx lastId newState
        let telegramBotState = { LastId = newLastId; State = newState }
        do! updateState connection creatorId telegramBotState }

  | None ->
    Async.singleton ()
