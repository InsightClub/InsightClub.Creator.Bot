module InsightClub.Creator.Bot.Render

open Bot
open System
open Funogram.Telegram
open System.Text.RegularExpressions


type User = Types.User
type Button = Types.InlineKeyboardButton

type Services = {
    getCourses: Page -> Count -> Async<(CourseId * CourseTitle) list>
    getBlocks: CourseId -> Page -> Count -> Async<(BlockId * BlockTitle) list>
    getCourseTitle: CourseId -> Async<CourseTitle>
    getCourseDesc: CourseId -> Async<CourseDesc>
}

let private cln s = Regex("\n[ ]+").Replace(s, "\n")
let private random = Random()

let randomEmoji () =
    let emojis =
        [| "🤷‍♂️"
           "😵‍💫"
           "🙄"
           "🤪"
           "🙀"
           "😭"
           "😣"
           "🥺"
           "😑"
           "💩" |]

    emojis[random.Next(emojis.Length)]

let private commands =
    cln$"{Dispatcher.new'} - Создать новый курс ⚡️
        {Dispatcher.edit} - Редактировать существующий курс 📝
        {Dispatcher.help} - Получить помощь (Вы сейчас здесь) 👀"

let private idleMsg (user: User) =
    function
    | Idle.Started ->
        let lastName =
            user.LastName
            |> Option.map ((+) " ")
            |> Option.defaultValue String.Empty

        cln$"Добро пожаловать в InsightClub.Bot, {user.FirstName} \
            {lastName}! ✨ С помощью этого бота Вы можете конструировать свои \
            курсы! 😎

            Отправьте /help для получения помощи 👀"

    | Idle.Helping ->
        cln$"Добро пожаловать в справку InsightClub.Creator.Bot! 🤖

            Этот бот имеет несколько режимов 🧞‍♂
            На данный момент он находится \
            в режиме ожидания. Все остальные режимы имеют вспомогательные \
            клавиатуры, которые помогут Вам легко разобраться в функционале.

            {commands}

            Учитывайте, что команда {Dispatcher.help} работает только в режиме ожидания. \
            В остальных режимах она не распознаётся, ибо их интерфейс поможет \
            Вам легко разобраться 🔥"

    | Idle.NoCourses ->
        cln$"У Вас пока нет курсов {randomEmoji ()}

            Создайте новый, отправив команду {Dispatcher.new'} 🤹‍♂️"

    | Idle.CreateCanceled ->
        cln$"Создание курса отменено 👌

            {commands}"

    | Idle.EditCanceled ->
        cln$"Редактирование отменено 👌

            {commands}"

    | Idle.ExitedEditing ->
        cln$"Как пожелаете 🧞‍♂
            Редактирование завершено.

            {commands}"

    | Idle.Error ->
        cln$"Неизвестная команда {randomEmoji ()}

            Отправьте {Dispatcher.help} для получения помощи 👀"

let private creatingCourseMsg =
    function
    | CreatingCourse.Started ->
        cln "Режим создания нового курса 🧚‍♂️

            Как Вы хотели бы назвать новый курс? 📝"

    | CreatingCourse.TitleError NonUnique ->
        cln "Курс с таким названием уже существует. 🤷‍♂️

            Пожалуйста, выберите другое."

    | CreatingCourse.TitleError TooLong ->
        cln "Название курса не должно превышать 100 символов. 😭

            Пожалуйста, выберите более короткое."

    | CreatingCourse.Error ->
        cln$"Неизвестная команда {randomEmoji ()}

            Режим создания нового курса.

            Как Вы хотели бы назвать новый курс? 📝"

let private editingCourseMsg =
    function
    | EditingCourse.CourseCreated ->
        cln "Курс создан! ✅

            Режим редактирования курса ✏️

            Для просмотра данных о курсе выберите соответствующий раздел. \
            Помимо просмотра, разделы позволяют также и \
            редактировать соответствующие данные курса."

    | EditingCourse.Editing ->
        cln "Режим редактирования курса ✏️

            Для просмотра данных о курсе выберите соответствующий раздел. \
            Помимо просмотра, разделы позволяют также и \
            редактировать соответствующие данные курса."

    | EditingCourse.TitleCanceled ->
        cln "Редактирование названия отменено 👌

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.TitleSet ->
        cln "Название курса обновлено! ✅

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.DescCanceled ->
        cln "Редактирование описания отменено 👌

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.DescSet ->
        cln "Описание курса обновлено! ✅

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.NewBlockCanceled ->
        cln "Создание нового блока отменено 👌

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.BlockCanceled ->
        cln "Редактирование блока отменено 👌

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.NoBlocks ->
        cln$"В этом курсе пока нет блоков {randomEmoji ()}

            Нажмите на кнопку «Добавить», чтоб добавить один или несколько блоков 🤹‍♂️

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

    | EditingCourse.Error ->
        cln$"Неизвестная команда {randomEmoji ()}

            Режим редактирования курса ✏️

            Выберите что Вы хотели бы сделать дальше."

let private editingTitleMsg title =
    function
    | EditingTitle.Started ->
        cln$"Редактируем название курса 🥸

            Текущее название курса:
            {title}

            Отправьте новое, чтоб изменить."

    | EditingTitle.TitleError NonUnique ->
        cln$"Курс с таким названием уже существует 🤷‍♂️

            Текущее название курса:
            {title}

            Отправьте новое, чтоб изменить."

    | EditingTitle.TitleError TooLong ->
        cln$"Название курса не должно превышать 100 символов. 😭

            Текущее название курса:
            {title}

            Отправьте новое, чтоб изменить."

    | EditingTitle.Error ->
        cln$"Неизвестная команда {randomEmoji ()}

            Текущее название курса:
            {title}

            Отправьте новое, чтоб изменить."

let private editingDescMsg desc =
    let desc =
        if String.IsNullOrEmpty desc then
            "У Вашего курса пока нет описания."

        else
            cln$"Текущее описание курса:
                {desc}"

    function
    | EditingDesc.Started ->
        cln$"Редактируем описание курса 👽

            {desc}

            Отправьте текст, чтоб изменить описание. \
            Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
            будут видеть в первую очередь, не считая названия курса."

    | EditingDesc.DescTooLong ->
        cln$"Описание курса не должно превышать 1000 символов. 🥺

            {desc}

            Отправьте текст, чтоб изменить описание курса. \
            Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
            будут видеть в первую очередь, не считая названия курса."

    | EditingDesc.Error ->
        cln$"Неизвестная команда {randomEmoji ()}

            {desc}

            Отправьте текст, чтоб изменить описание курса. \
            Постарайтесь сделать его понятным и читаемым. Это то, что Ваши клиенты \
            будут видеть в первую очередь, не считая названия курса."

let private listingCoursesMsg page count courseCount msg =
    let m s =
        match msg with
        | ListingCourses.Started -> s

        | ListingCourses.Error ->
            cln$"Неизвестная команда. {randomEmoji ()}

                {s}"

    let min = page * count + 1
    let max = page * count + courseCount

    let text =
        if min = max then
            $"Курс № {min}"

        else
            $"Курсы с № {min} по № {max}"

    cln (m text)

let private creatingBlockMsg =
    function
    | CreatingBlock.Started ->
        cln "Режим создания блока 💫

            Отправьте заголовок будущего блока."

    | CreatingBlock.TitleReserved ->
        cln "Блок с таким заголовком уже существует. 🤷‍♂️

            Пожалуйста, выберите другой."

    | CreatingBlock.Error ->
        cln$"Неизвестная команда {randomEmoji ()}

            Режим создания блока.

            Как Вы хотели бы озаглавить новый блок? 📝"

let private editingBlockMsg title =
    function
    | EditingBlock.Started ->
        cln$"{title}

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

        cln$"{title}

            {addedMsg}

            Отправьте ещё текст, фото, аудио, видео, голос, документ \
            или короткое видео, чтоб добавить его в конец блока."

    | EditingBlock.Cleaned ->
        cln$"{title}

            Блок очищен.

            Режим редактирования блока ✨

            Отправьте текст, фото, аудио, видео, голос, документ \
            или короткое видео, чтоб добавить его в конец блока."

    | EditingBlock.Error ->
        cln$"{title}

            Неизвестная команда {randomEmoji ()}

            Режим редактирования блока.

            Отправьте текст, фото, аудио, видео, голос, документ \
            или короткое видео, чтоб добавить его в конец блока."

let private listingBlocksMsg page count blocksCount msg =
    let m s =
        match msg with
        | ListingBlocks.Started -> s

        | ListingBlocks.Error ->
            cln$"Неизвестная команда. {randomEmoji ()}

                {s}"

    let min = page * count + 1
    let max = page * count + blocksCount

    let text =
        if min = max then
            $"Блок № {min}"

        else
            $"Блоки с № {min} по № {max}"

    cln (m text)

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

let private button text command : Button = {
    Text = text
    CallbackData = Some command
    Url = None
    Pay = None
    LoginUrl = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None
    WebApp = None
}

let state services user state =
    async {
        match state with
        | Inactive ->
            return
                String.Empty, None

        | Idle msg ->
            return
                idleMsg user msg, None

        | CreatingCourse msg ->
            return
                creatingCourseMsg msg,
                Some [
                    [
                        button Button.cancel Dispatcher.cancel
                    ]
                ]

        | EditingCourse (_, msg) ->
            return
                editingCourseMsg msg,
                Some [
                    [
                        button Button.title Dispatcher.title
                        button Button.desc Dispatcher.desc
                    ]
                    [
                        button Button.add Dispatcher.add
                        button Button.edit Dispatcher.edit
                    ]
                    [
                        button Button.exit Dispatcher.exit
                    ]
                ]

        | EditingTitle (courseId, msg) ->
            let! title = services.getCourseTitle courseId

            return
                editingTitleMsg title msg,
                Some [
                    [
                        button Button.cancel Dispatcher.cancel
                    ]
                ]

        | EditingDesc (courseId, msg) ->
            let! desc = services.getCourseDesc courseId

            return
                editingDescMsg desc msg,
                Some [
                    [
                        button Button.cancel Dispatcher.cancel
                    ]
                ]

        | ListingCourses subState ->
            let! courses = services.getCourses subState.Page subState.Count

            let coursesCount = List.length courses

            return
                listingCoursesMsg subState.Page subState.Count coursesCount subState.Msg,
                Some [
                    for id, title in courses do
                        yield [
                            button title $"{Dispatcher.edit} {id}"
                        ]

                        yield [
                            button Button.prev Dispatcher.prev
                            button Button.next Dispatcher.next
                        ]

                        yield [
                            button Button.exit Dispatcher.exit
                        ]
                ]

        | CreatingBlock subState ->
            return
                creatingBlockMsg subState.Msg,
                Some [
                    [
                        button Button.cancel Dispatcher.cancel
                    ]
                ]

        | EditingBlock subState ->
            return
                editingBlockMsg subState.Title subState.Msg,
                Some [
                    [
                        button Button.insert Dispatcher.ignore
                    ]
                    [
                        button Button.before Dispatcher.before
                        button Button.after Dispatcher.after
                    ]
                    [
                        button Button.move Dispatcher.ignore
                    ]
                    [
                        button Button.movePrev Dispatcher.prev
                        button Button.moveNext Dispatcher.next
                    ]
                    [
                        button Button.show Dispatcher.show
                        button Button.clean Dispatcher.clean
                    ]
                    [
                        button Button.back Dispatcher.back
                    ]
                ]

        | ListingBlocks subState ->
            let! blocks = services.getBlocks subState.CourseId subState.Page subState.Count

            let blocksCount = List.length blocks

            return
                listingBlocksMsg subState.Page subState.Count blocksCount subState.Msg,
                Some [
                    for i, (id, title) in List.indexed blocks do
                        yield [
                            button
                                $"{subState.Page * subState.Count + i + 1}. {title}"
                                $"{Dispatcher.edit} {id}"
                        ]

                        yield [
                            button Button.prev Dispatcher.prev
                            button Button.next Dispatcher.next
                        ]

                        yield [
                            button Button.back Dispatcher.back
                        ]
                ]
    }

let queryEffect =
    function
    | Some (Dispatcher.ShowContent []) ->
        [], Some "Этот блок пока что пуст. Добавьте контент."

    | Some (Dispatcher.ShowContent contents) ->
        contents, None

    | Some Dispatcher.BeginningReached ->
        [], Some "Вы дошли до начала."

    | Some Dispatcher.EndingReached ->
        [], Some "Вы дошли до конца."

    | Some Dispatcher.BlockEmpty ->
        [], Some "Очищение не требуется. Блок пуст."

    | None ->
        [], None
