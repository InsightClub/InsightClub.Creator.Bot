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
  let title = "/title"
  let desc = "/desc"
  let show = "/show"
  let edit = "/edit"
  let prev = "/prev"
  let next = "/next"

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

    | Some { Text = Some (Command Command.edit) } ->
      Some (Idle.EditCourse 5)

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
    | Some { Data = Some (Command Command.title) } ->
      Some EditingCourse.EditTitle

    | Some { Data = Some (Command Command.desc) } ->
      Some EditingCourse.EditDesc

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

  let getEditingDesc () =
    match ctx.Update with
    | { CallbackQuery = Some { Data = Some (Command Command.show) } } ->
      Some EditingDesc.Show

    | { CallbackQuery = Some { Data = Some (Command Command.cancel) } } ->
      Some EditingDesc.Cancel

    | { Message = Some { Text = Some (PlainText courseDesc) } } ->
      Some <| EditingDesc.SetDesc courseDesc

    | _ ->
      None

  let getListingCourses () =
    match ctx.Update.CallbackQuery with
    | Some { Data = Some (CommandParam Command.edit courseId) } ->
      Some (ListingCourses.Select courseId)

    | Some { Data = Some (Command Command.prev) } ->
      Some ListingCourses.Prev

    | Some { Data = Some (Command Command.next) } ->
      Some ListingCourses.Next

    | Some { Data = Some (Command Command.exit) } ->
      Some ListingCourses.Exit

    | _ ->
      None

  { getInactive = getInactive
    getIdle = getIdle
    getCreatingCourse = getCreatingCourse
    getEditingCourse = getEditingCourse
    getEditingTitle = getEditingTitle
    getEditingDesc = getEditingDesc
    getListingCourses = getListingCourses }

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

let answerCallbackQuery config (query: CallbackQuery) text =
  Api.answerCallbackQueryBase
    (Some query.Id) text None None None
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

// Text utils
let c s = Regex("\n[ ]+").Replace(s, "\n")
let random = Random()
let randomEmoji () =
  let emojis =
    [| "🤷‍♂️"; "😵‍💫"; "🙄"; "🤪"; "🙀"
       "😭"; "😣"; "🥺"; "😑"; "💩" |]

  emojis.[ random.Next(emojis.Length) ]

// Messages
let idleMsg (user: User) =
  function
  | Idle.Started ->
    let lastName =
      user.LastName
      |> Option.map ((+) " ")
      |> Option.defaultValue ""

    c$"Добро пожаловать в InsightClub.Creator.Bot, {user.FirstName} \
      {lastName}! ✨ С помощью этого бота Вы можете конструировать свои \
      курсы! 😎

      Отправьте /help для получения помощи 👀"

  | Idle.Helping ->
    c$"Добро пожаловать в справку InsightClub.Creator.Bot! 🤖

      Этот бот имеет несколько режимов 🧞‍♂️ На данный момент он находится \
      в режиме ожидания. Все остальные режимы имеют вспомогательные \
      клавиатуры, которые помогут Вам легко разобраться в функционале.

      {Command.new'} - Создать новый курс ⚡️
      {Command.edit} - Редактировать существующий курс 📝
      {Command.help} - Получить помощь (Вы сейчас здесь) 👀

      Учитывайте, что команда {Command.help} работает только в режиме ожидания. \
      В остальных режимах она не распознаётся, ибо их интерфейс поможет \
      Вам легко разобраться 🔥"

  | Idle.NoCourses ->
    c$"У Вас пока нет курсов {randomEmoji ()}
      Создайте новый, отправив команду {Command.new'} 🤹‍♂️"

  | Idle.CreateCanceled ->
    "Создание курса отменено 👌"

  | Idle.EditCanceled ->
    "Редактирование отменено 👌"

  | Idle.ExitedEditing ->
    "Как пожелаете 🧞‍♂️ Редактирование завершено."

  | Idle.Error ->
    c$"Неизвестная команда {randomEmoji ()}
      Отправьте {Command.help} для получения помощи 👀"

let creatingCourseMsg =
  function
  | CreatingCourse.Started ->
    c "Режим создания нового курса 🧚‍♂️
      Как Вы хотели бы назвать новый курс? 📝"

  | CreatingCourse.TitleReserved ->
    c "Курс с таким названием уже существует. 🤷‍♂️
      Пожалуйста, выберите другое."

  | CreatingCourse.Error ->
    c$"Неизвестная команда {randomEmoji ()}

      Режим создания нового курса.
      Как Вы хотели бы назвать новый курс? 📝"

let editingCourseMsg =
  function
  | EditingCourse.CourseCreated ->
    c "Курс создан! ✅

      Режим редактирования курса ✏️
      Для просмотра данных о курсе выберите соответствующий раздел. \
      Помимо просмотра, разделы позволяют также и \
      редактировать соответствующие данные курса."

  | EditingCourse.Editing ->
    c "Режим редактирования курса ✏️
      Для просмотра данных о курсе выберите соответствующий раздел. \
      Помимо просмотра, разделы позволяют также и \
      редактировать соответствующие данные курса."

  | EditingCourse.TitleCanceled ->
    c "Редактирование названия отменено 👌

      Режим редактирования курса ✏️
      Выберите что Вы хотели бы сделать дальше."

  | EditingCourse.TitleSet ->
    c "Название курса обновлено! ✅

      Режим редактирования курса ✏️
      Выберите что Вы хотели бы сделать дальше."

  | EditingCourse.DescCanceled ->
    c "Редактирование описания отменено 👌

      Режим редактирования курса ✏️
      Выберите что Вы хотели бы сделать дальше."

  | EditingCourse.DescSet ->
    c "Описание курса обновлено! ✅

      Режим редактирования курса ✏️
      Выберите что Вы хотели бы сделать дальше."

  | EditingCourse.Error ->
    c$"Неизвестная команда {randomEmoji ()}

      Режим редактирования курса ✏️
      Выберите что Вы хотели бы сделать дальше."

let editingTitleMsg title =
  function
  | EditingTitle.Started ->
    c$"Редактируем название курса 🥸

      Текущее название курса: {title}
      Отправьте новое, чтоб изменить."

  | EditingTitle.TitleReserved ->
    c$"Курс с таким названием уже существует 🤷‍♂️

      Текущее название курса: {title}
      Отправьте новое, чтоб изменить."

  | EditingTitle.Error ->
    c$"Неизвестная команда {randomEmoji ()}

      Текущее название курса: {title}
      Отправьте новое, чтоб изменить."

let editingDescMsg =
  function
  | EditingDesc.Started ->
    c "Редактируем описание курса 👽

      Отправьте текст, чтоб изменить описание.
      Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
      будут видеть в первую очередь, не считая названия курса."

  | EditingDesc.Error ->
    c$"Неизвестная команда {randomEmoji ()}

      Отправьте текст, чтоб изменить описание курса.
      Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
      будут видеть в первую очередь, не считая названия курса."

let listingCoursesMsg page count courseCount msg =
  let m s =
    match msg with
    | ListingCourses.Started ->
      s

    | ListingCourses.Error ->
      c$"Неизвестная команда. {randomEmoji ()}

        {s}"

  let min = page * count + 1
  let max = page * count + courseCount

  if min = max
  then $"Курс № {min}"
  else $"Курсы с № {min} по № {max}"
  |> m
  |> c

module Button =
  let cancel = "Отмена ❌"
  let exit = "Выход 🚪"
  let title = "Название ✏️"
  let desc = "Описание 🖋"
  let show = "Показать 👁"
  let prev = "Назад ⬅️"
  let next = "Вперёд ➡️"

// Response for state
let handleState (ctx: UpdateContext) connection creatorId lastId state = async {
  // onUpdate must ensure user is available, so this call is safe
  let user = Option.get <| getUser ctx

  let! message, keyboard = async {
    match state with
    | Inactive ->
      return String.Empty, None

    | Idle msg ->
      return idleMsg user msg, None

    | CreatingCourse msg ->
      return
        creatingCourseMsg msg,
        Some [ [ button Button.cancel Command.cancel ] ]

    | EditingCourse (_, msg) ->
      return
        editingCourseMsg msg,
        Some
          [ [ button Button.title Command.title
              button Button.desc Command.desc ]
            [ button Button.exit Command.exit ] ]

    | EditingTitle (_, title, msg) ->
      return
        editingTitleMsg title msg,
        Some [ [ button Button.cancel Command.cancel ] ]

    | EditingDesc (_, msg) ->
      return
        editingDescMsg msg,
        Some [ [ button Button.show Command.show
                 button Button.cancel Command.cancel ] ]

    | ListingCourses (page, count, msg) ->
      let! courses = Repo.getCourses connection creatorId page count

      return
        listingCoursesMsg page count (List.length courses) msg,
        ( courses
          |> List.map
            ( fun (id, title) ->
                [ button title $"{Command.edit} {id}" ] ) )
        @ [ [ button Button.prev Command.prev
              button Button.next Command.next ]
            [ button Button.exit Command.exit ] ]
        |> Some }

  match ctx.Update with
  | { Message = Some _ } ->
    let! _ =
      removeLastMarkupMaybe ctx.Config lastId user.Id
      |> Async.StartChild

    return!
      sendMessage ctx.Config lastId user.Id message keyboard

  | { CallbackQuery = Some query } ->
    let! _ =
      answerCallbackQuery ctx.Config query None
      |> Async.StartChild

    if lastId.IsSome then
      let! _ =
        editMessage ctx.Config lastId user.Id message keyboard

      return
        keyboard
        |> Option.bind (always lastId)
    else
      return!
        sendMessage ctx.Config lastId user.Id message keyboard

  | _ ->
    return lastId }

// Response for effect
let handleEffect (ctx: UpdateContext) lastId =
  // onUpdate must ensure user is available, so this call is safe
  let user = Option.get <| getUser ctx
  let config = ctx.Config

  function
  | Nothing ->
    Async.singleton lastId

  | ShowDesc text -> async {
    let! _ =
      removeLastMarkupMaybe config lastId user.Id
      |> Async.StartChild

    let text =
      if String.IsNullOrEmpty text
      then $"У Вашего курса пока нет описания {randomEmoji ()}"
      else text

    do!
      Api.sendMessage user.Id text
      |> Api.api config
      |> Async.Ignore

    return None }

  | InformNoPrev -> async {
    match ctx.Update.CallbackQuery with
    | Some query ->
      let! _ =
        answerCallbackQuery ctx.Config query (Some "Вы дошли до минимума")
        |> Async.StartChild

      return lastId

    | None ->
      return lastId }

  | InformNoNext -> async {
    match ctx.Update.CallbackQuery with
    | Some query ->
      let! _ =
        answerCallbackQuery ctx.Config query (Some "Вы дошли до максимума")
        |> Async.StartChild

      return lastId

    | None ->
      return lastId }

// Main function
let onUpdate getConnection ctx =
  let update (user: User) = async {
    use connection = getConnection ()
    let! creatorId, lastId, state = State.get connection user.Id
    let services = Services.get connection creatorId
    let commands = getCommands ctx
    let! (state, effect) = update services commands state
    let! lastId = handleEffect ctx lastId effect
    let! lastId = handleState ctx connection creatorId lastId state
    do! State.update connection creatorId lastId state }

  ctx
  |> getUser // Ensure user is available
  |> Option.map update
  |> Option.defaultValue Async.doNothing
