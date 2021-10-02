module InsightClub.Creator.Bot.Api

open Core
open System
open Funogram
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open System.Text.RegularExpressions


// Commands
module Command =
  let start = "/start"
  let help = "/help"
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
    match ctx.Update.Message with
    | Some { Text = Some (Command Command.help) } ->
      Some Idle.Help

    | Some { Text = Some (Command Command.new') } ->
      Some Idle.CreateCourse

    | _ ->
      None

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

let button text command =
  { Text = text
    CallbackData = Some command
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

// Text utils
let c s = Regex("\n[ ]+").Replace(s, "\n")
let f = sprintf
let r = Random()
let randomError () =
  let emojees =
    [| "🤷‍♂️"; "😵‍💫"; "🙄"; "🤪"; "🙀"
       "😭"; "😣"; "🥺"; "😑"; "💩" |]

  emojees.[ r.Next(emojees.Length) ]

// Messages
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
    let lastName =
      user.LastName
      |> Option.map ((+) " ")
      |> Option.defaultValue ""

    f "Добро пожаловать в InsightClub.Creator.Bot, %s%s! ✨ \
      С помощью этого бота Вы можете конструировать свои курсы! 😎

      Отправьте /help для получения помощи. 👀" user.FirstName lastName
    |> c

  | Idle.Helping ->
    c " Добро пожаловать в справку InsightClub.Creator.Bot! 🤖

      Этот бот имеет несколько режимов. 🧞‍♂️ На данный момент он находится \
      в режиме ожидания. Все остальные режимы имеют вспомогательные \
      клавиатуры, которые помогут Вам легко разобраться в функционале.

      /new - Создать новый курс ⚡️
      /help - Получить помощь (Вы сейчас здесь) 👀

      Учитывайте, что команда /help работает только в режиме ожидания. \
      В остальных режимах она не распознаётся, ибо их интерфейс поможет \
      Вам легко разобраться. 🔥"

  | Idle.CourseCanceled ->
    "Создание курса отменено. 👌"

  | Idle.ExitedEditing ->
    "Как пожелаете. 🧞‍♂️ Редактирование завершено."

  | Idle.Error ->
    f "Неизвестная команда. %s
      Отправьте /help для получения помощи. 👀" (randomError())
    |> c

let creatingCourseMessage =
  function
  | CreatingCourse.Started ->
    c "Режим создания нового курса. 🧚‍♂️
      Как Вы хотели бы назвать новый курс? 📝"

  | CreatingCourse.TitleReserved ->
    c "Курс с таким названием уже существует. 🤷‍♂️
      Пожалуйста, выберите другое."

  | CreatingCourse.Error ->
    f "Неизвестная команда. %s

      Режим создания нового курса.
      Как Вы хотели бы назвать новый курс? 📝" (randomError())
    |> c

let editingCourseMessage =
  function
  | EditingCourse.CourseCreated ->
    c "Курс создан! ✅

      Режим редактирования курса. ✏️
      Для просмотра данных о курсе выберите соответствующий раздел. \
      Помимо просмотра, разделы позволяют также и \
      редактировать соответствующие данные курса."

  | EditingCourse.TitleCanceled ->
    c "Редактирование названия отменено. 👌

      Режим редактирования курса. ✏️
      Выберите что Вы хотели бы сделать дальше."

  | EditingCourse.TitleSet ->
    c "Название курса обновлено! ✅

      Режим редактирования курса. ✏️
      Выберите что Вы хотели бы сделать дальше."

  | EditingCourse.Error ->
    f "Неизвестная команда. %s

      Режим редактирования курса. ✏️
      Выберите что Вы хотели бы сделать дальше." (randomError())
    |> c

let editingTitleMessage title =
  function
  | EditingTitle.Started ->
    f "Редактируем название курса. 🥸

      Текущее название курса: %s
      Отправьте новое, чтоб изменить." title
    |> c

  | EditingTitle.TitleReserved ->
    f "Курс с таким названием уже существует. 🤷‍♂️

      Текущее название курса: %s
      Отправьте новое, чтоб изменить." title
    |> c

  | EditingTitle.Error ->
    f "Неизвестная команда. %s

      Текущее название курса: %s
      Отправьте новое, чтоб изменить." (randomError()) title
    |> c

module Button =
  let cancel = "Отмена ❌"
  let exit = "Выход 🚪"
  let title = "Название ✏️"

// Response
let respond (ctx: UpdateContext) lastId state =
  // updateArrived must ensure user is present, so this call is safe
  let user = Option.get <| getUser ctx

  let message, keyboard =
    match state with
    | Inactive ->
      String.Empty, None

    | Idle data ->
      idleMessage user data, None

    | CreatingCourse data ->
      creatingCourseMessage data,
      Some [ [ button Button.cancel Command.cancel ] ]

    | EditingCourse (_, data) ->
      editingCourseMessage data,
      Some
        [ [ button Button.title Command.edit ]
          [ button Button.exit Command.exit ] ]

    | EditingTitle (_, title, data) ->
      editingTitleMessage title data,
      Some [ [ button Button.cancel Command.cancel ] ]

  match ctx.Update with
  | { Message = Some _ } -> async {
    let! _ =
      removeLastMarkupMaybe ctx.Config lastId user.Id
      |> Async.StartChild

    return!
      sendMessage ctx.Config lastId user.Id message keyboard }

  | { CallbackQuery = Some query } -> async {
    let! _ =
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
  let update (user: User) = async {
    use connection = getConnection ()
    let! creatorId, lastId, botState = State.get connection user.Id
    let services = Services.get connection creatorId
    let commands = getCommands ctx
    let callback = Async.singleton
    let! newBotState = update services commands callback botState
    let! newLastId = respond ctx lastId newBotState
    let state = State.create newLastId newBotState
    do! State.update connection creatorId state }

  ctx
  |> getUser // Ensure user is present
  |> Option.map update
  |> Option.defaultValue Async.doNothing
