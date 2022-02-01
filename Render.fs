module InsightClub.Creator.Bot.Render

open Core
open System
open Funogram.Telegram
open System.Text.RegularExpressions


type User = Types.User
type Button = Types.InlineKeyboardButton

type Services =
  { getCourses: Page -> Count -> Async<(CourseId * CourseTitle) list>
    getBlocks: CourseId -> Page -> Count -> Async<(BlockId * BlockTitle) list>
    getCourseTitle: CourseId -> Async<CourseTitle>
    getCourseDesc: CourseId -> Async<CourseDesc> }

let private c s = Regex("\n[ ]+").Replace(s, "\n")
let private random = Random()
let randomEmoji () =
  let emojis =
    [| "🤷‍♂️"; "😵‍💫"; "🙄"; "🤪"; "🙀"
       "😭"; "😣"; "🥺"; "😑"; "💩" |]

  emojis.[ random.Next(emojis.Length) ]

let private commands =
  c$"{Commands.new'} - Создать новый курс ⚡️
    {Commands.edit} - Редактировать существующий курс 📝
    {Commands.help} - Получить помощь (Вы сейчас здесь) 👀"

let private idleMsg (user: User) = function
| Idle.Started ->
  let lastName =
    user.LastName
    |> Option.map ((+) " ")
    |> Option.defaultValue String.Empty

  c$"Добро пожаловать в InsightClub.Creator.Bot, {user.FirstName} \
    {lastName}! ✨ С помощью этого бота Вы можете конструировать свои \
    курсы! 😎

    Отправьте /help для получения помощи 👀"

| Idle.Helping ->
  c$"Добро пожаловать в справку InsightClub.Creator.Bot! 🤖

    Этот бот имеет несколько режимов 🧞‍♂️ На данный момент он находится \
    в режиме ожидания. Все остальные режимы имеют вспомогательные \
    клавиатуры, которые помогут Вам легко разобраться в функционале.

    {commands}

    Учитывайте, что команда {Commands.help} работает только в режиме ожидания. \
    В остальных режимах она не распознаётся, ибо их интерфейс поможет \
    Вам легко разобраться 🔥"

| Idle.NoCourses ->
  c$"У Вас пока нет курсов {randomEmoji ()}
    Создайте новый, отправив команду {Commands.new'} 🤹‍♂️"

| Idle.CreateCanceled ->
  c$"Создание курса отменено 👌

    {commands}"

| Idle.EditCanceled ->
  c$"Редактирование отменено 👌

    {commands}"

| Idle.ExitedEditing ->
  c$"Как пожелаете 🧞‍♂️ Редактирование завершено.

    {commands}"

| Idle.Error ->
  c$"Неизвестная команда {randomEmoji ()}
    Отправьте {Commands.help} для получения помощи 👀"

let private creatingCourseMsg = function
| CreatingCourse.Started ->
  c "Режим создания нового курса 🧚‍♂️

    Как Вы хотели бы назвать новый курс? 📝"

| CreatingCourse.TitleError NonUnique ->
  c "Курс с таким названием уже существует. 🤷‍♂️

    Пожалуйста, выберите другое."

| CreatingCourse.TitleError TooLong ->
  c "Название курса не должно превышать 100 символов. 😭

    Пожалуйста, выберите более короткое."

| CreatingCourse.Error ->
  c$"Неизвестная команда {randomEmoji ()}

    Режим создания нового курса.

    Как Вы хотели бы назвать новый курс? 📝"

let private editingCourseMsg = function
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

| EditingCourse.NewBlockCanceled ->
  c "Создание нового блока отменено 👌

    Режим редактирования курса ✏️

    Выберите что Вы хотели бы сделать дальше."

| EditingCourse.BlockCanceled ->
  c "Редактирование блока отменено 👌

    Режим редактирования курса ✏️

    Выберите что Вы хотели бы сделать дальше."

| EditingCourse.NoBlocks ->
  c$"В этом курсе пока нет блоков {randomEmoji ()}
    Нажмите на кнопку «Добавить», чтоб добавить один или несколько блоков 🤹‍♂️

    Режим редактирования курса ✏️

    Выберите что Вы хотели бы сделать дальше."

| EditingCourse.Error ->
  c$"Неизвестная команда {randomEmoji ()}

    Режим редактирования курса ✏️

    Выберите что Вы хотели бы сделать дальше."

let private editingTitleMsg title = function
| EditingTitle.Started ->
  c$"Редактируем название курса 🥸

    Текущее название курса:
    {title}

    Отправьте новое, чтоб изменить."

| EditingTitle.TitleError NonUnique ->
  c$"Курс с таким названием уже существует 🤷‍♂️

    Текущее название курса:
    {title}

    Отправьте новое, чтоб изменить."

| EditingTitle.TitleError TooLong ->
  c$"Название курса не должно превышать 100 символов. 😭

    Текущее название курса:
    {title}

    Отправьте новое, чтоб изменить."

| EditingTitle.Error ->
  c$"Неизвестная команда {randomEmoji ()}

    Текущее название курса:
    {title}

    Отправьте новое, чтоб изменить."

let private editingDescMsg desc =
  let desc =
    if String.IsNullOrEmpty desc then
      "У Вашего курса пока нет описания."
    else
      c$"Текущее описание курса:
        {desc}"

  function
  | EditingDesc.Started ->
    c$"Редактируем описание курса 👽

      {desc}

      Отправьте текст, чтоб изменить описание.
      Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
      будут видеть в первую очередь, не считая названия курса."

  | EditingDesc.Error ->
    c$"Неизвестная команда {randomEmoji ()}

      Отправьте текст, чтоб изменить описание курса.

      Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
      будут видеть в первую очередь, не считая названия курса."

let private listingCoursesMsg page count courseCount msg =
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

let private creatingBlockMsg = function
| CreatingBlock.Started ->
  c "Режим создание блока 💫

    Отправьте заголовок будущего блока."

| CreatingBlock.TitleReserved ->
  c "Блок с таким заголовком уже существует. 🤷‍♂️

     Пожалуйста, выберите другой."

| CreatingBlock.Error ->
  c$"Неизвестная команда {randomEmoji ()}

    Режим создания блока.

    Как Вы хотели бы озаглавить новый блок? 📝"

let private editingBlockMsg title = function
| EditingBlock.Started ->
  c$"{title}

    Режим редактирования блока ✨

    Отправьте текст, фото, аудио, видео, голос, документ \
    или короткое видео, чтоб добавить его в конец блока."

| EditingBlock.ContentAdded content ->
  let addedMsg =
    match content with
    | Text _ -> "Текст добавлен."
    | Photo _ -> "Фото добавлено."
    | Audio _ -> "Аудио добавлено."
    | Video _ -> "Видео добавлено."
    | Voice _ -> "Голос добавлен."
    | Document _ -> "Документ добавлен."
    | VideoNote _ -> "Короткое видео добавлено."

  c$"{title}

  {addedMsg}

  Отправьте ещё текст, фото, аудио, видео, голос, документ \
  или короткое видео, чтоб добавить его в конец блока."

| EditingBlock.Cleaned ->
  c$"{title}

    Блок очищен.

    Режим редактирования блока ✨

    Отправьте текст, фото, аудио, видео, голос, документ \
    или короткое видео, чтоб добавить его в конец блока."

| EditingBlock.Error ->
  c$"{title}

    Неизвестная команда {randomEmoji ()}

    Режим редактирования блока.

    Отправьте текст, фото, аудио, видео, голос, документ \
    или короткое видео, чтоб добавить его в конец блока."

let private listingBlocksMsg page count blocksCount msg =
  let m s =
    match msg with
    | ListingBlocks.Started ->
      s

    | ListingBlocks.Error ->
      c$"Неизвестная команда. {randomEmoji ()}

        {s}"

  let min = page * count + 1
  let max = page * count + blocksCount

  if min = max
  then $"Блок № {min}"
  else $"Блоки с № {min} по № {max}"
  |> m
  |> c

module private Button =
  let cancel = "Отмена ❌"
  let exit = "Выход 🚪"
  let title = "Название ✏️"
  let desc = "Описание 🖋"
  let show = "Показать 👁"
  let prev = "⬅️"
  let next = "➡️"
  let add = "Добавить 📄"
  let edit = "Редактировать 🗃"
  let back = "Назад 🚪"
  let before = "До ⬅️"
  let after = "После ➡️"
  let insert = "--- Вставить блок ---"
  let move = "--- Перейти к блоку ---"
  let movePrev = "Предыдущий ⬅️"
  let moveNext = "Следующий ➡️"
  let clean = "Очистить 🗑"

let private button text command : Button =
  { Text = text
    CallbackData = Some command
    Url = None
    Pay = None
    LoginUrl = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None }

let state services user state = async {
  match state with
  | Inactive ->
    return String.Empty, None

  | Idle msg ->
    return idleMsg user msg, None

  | CreatingCourse msg ->
    return
      creatingCourseMsg msg,
      Some [ [ button Button.cancel Commands.cancel ] ]

  | EditingCourse (_, msg) ->
    return
      editingCourseMsg msg,
      Some
        [ [ button Button.title Commands.title
            button Button.desc  Commands.desc ]
          [ button Button.add   Commands.add
            button Button.edit  Commands.edit ]
          [ button Button.exit  Commands.exit ] ]

  | EditingTitle (courseId, msg) ->
    let! title = services.getCourseTitle courseId

    return
      editingTitleMsg title msg,
      Some [ [ button Button.cancel Commands.cancel ] ]

  | EditingDesc (courseId, msg) ->
    let! desc = services.getCourseDesc courseId

    return
      editingDescMsg desc msg,
      Some
        [ [ button Button.cancel Commands.cancel ] ]

  | ListingCourses (page, count, msg) ->
    let! courses = services.getCourses page count

    return
      listingCoursesMsg page count (List.length courses) msg,
      Some
        [ for (id, title) in courses do
            yield [ button title $"{Commands.edit} {id}" ]

          yield [ button Button.prev Commands.prev
                  button Button.next Commands.next ]

          yield [ button Button.exit Commands.exit ] ]

  | CreatingBlock (_, _, msg) ->
    return
      creatingBlockMsg msg,
      Some [ [ button Button.cancel Commands.cancel ] ]

  | EditingBlock (_, _, _, title, msg) ->
    return
      editingBlockMsg title msg,
      Some
        [ [ button Button.insert    Commands.nothing ]
          [ button Button.before    Commands.before
            button Button.after     Commands.after   ]
          [ button Button.move      Commands.nothing ]
          [ button Button.movePrev  Commands.prev
            button Button.moveNext  Commands.next    ]
          [ button Button.show      Commands.show
            button Button.clean     Commands.clean   ]
          [ button Button.back      Commands.back    ] ]

  | ListingBlocks (courseId, page, count, msg) ->
    let! blocks = services.getBlocks courseId page count

    return
      listingBlocksMsg page count (List.length blocks) msg,
      Some
        [ for (i, (id, title)) in List.indexed blocks do
            yield
              [ button
                  $"{page * count + i + 1}. {title}"
                  $"{Commands.edit} {id}" ]

          yield [ button Button.prev Commands.prev
                  button Button.next Commands.next ]

          yield [ button Button.back Commands.back ] ] }

let queryEffect = function
| Some (Commands.ShowContent [ ]) ->
  [ ], Some "Этот блок пока что пуст. Добавьте контент."

| Some (Commands.ShowContent contents) ->
  contents, None

| Some Commands.InformMin ->
  [ ], Some "Вы дошли до начала."

| Some Commands.InformMax ->
  [ ], Some "Вы дошли до конца."

| Some Commands.InformEmpty ->
  [ ], Some "Очищение не требуется. Блок пуст."

| None ->
  [ ], None
