module InsightClub.Creator.Bot.Api

open Core
open Funogram
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
  // updateArrived must ensure user is present, so this call is safe
  let user = getUser ctx |> Option.get
  let api r = Api.api ctx.Config r

  let removeLastMarkupMaybe () =
    lastId
    |> Option.map
      ( fun lastId ->
          Api.editMessageReplyMarkupBase
            (Some <| Int user.Id) (Some lastId) None None
          |> api
          |> Async.Ignore )
    |> Option.defaultValue Async.doNothing

  let answerCallbackMaybe () =
    ctx.Update.CallbackQuery
    |> Option.map
      ( fun query ->
          Api.answerCallbackQueryBase (Some query.Id) (Some "") None None None
          |> api
          |> Async.Ignore )
    |> Option.defaultValue Async.doNothing

  let sendMessage text =
    async
      { do! removeLastMarkupMaybe ()
        do! answerCallbackMaybe ()
        let! _ = Api.sendMessage user.Id text |> api
        return None }

  let sendMessageMarkup text markup =
    async
      { do! removeLastMarkupMaybe ()
        do! answerCallbackMaybe ()
        let! r = Api.sendMessageMarkup user.Id text markup |> api
        return
          match r with
          | Ok m -> Some m.MessageId
          | Error _ -> None }

  let editLastMessage text markup =
    match lastId with
    | Some lastId ->
      async
        { do! answerCallbackMaybe ()

          let! _ =
            Api.editMessageTextBase
              (Some <| Int user.Id) (Some lastId) None text None None markup
            |> api

          return
            markup
            |> Option.map (always lastId) }

    | None ->
      Async.singleton None

  function
  | Inactive ->
    Async.singleton None

  | Idle Idle.Started ->
    sendMessage <| Message.greeting user.FirstName user.LastName

  | Idle Idle.CourseCanceled ->
    editLastMessage Message.courseCanceled None

  | Idle Idle.ExitedEditing ->
    editLastMessage Message.exitedEditing None

  | Idle Idle.Error ->
    sendMessage Message.error

  | CreatingCourse CreatingCourse.Started ->
    [ [ button Message.cancel cancelCourse ] ]
    |> markup
    |> sendMessageMarkup Message.courseStarted

  | CreatingCourse CreatingCourse.TitleReserved ->
    [ [ button Message.cancel cancelCourse ] ]
    |> markup
    |> sendMessageMarkup Message.titleReserved

  | CreatingCourse CreatingCourse.Error ->
    [ [ button Message.cancel cancelCourse ] ]
    |> markup
    |> sendMessageMarkup Message.error

  | EditingCourse _ ->
    [ [ button Message.exit exitEditing ] ]
    |> markup
    |> sendMessageMarkup Message.editingCourse

// Main function
let updateArrived getConnection ctx =
  ctx
  |> getUser // Ensure user is present
  |> Option.map
      ( fun user ->
          async
            { use connection = getConnection ()
              let! creatorId, lastId, state = getState connection user.Id
              let services = getServices connection creatorId
              let commands = getCommands ctx
              let callback = Async.singleton
              let! newState = update services commands callback state
              let! newLastId = respond ctx lastId newState
              let telegramBotState = { LastId = newLastId; State = newState }
              do! updateState connection creatorId telegramBotState } )

  |> Option.defaultValue Async.doNothing
