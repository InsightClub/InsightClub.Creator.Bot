module InsightClub.Creator.Bot.Message

open System.Text.RegularExpressions


// Cleaning
let c s = Regex("\n[ ]+").Replace(s, "\n")
let f = sprintf

// Line
let ln = "\n"
let doubleLn = ln + ln

// Button texts
let cancel = "Отмена"
let exit = "Выход"

// Messages
let greeting firstName lastNameOption =
  let lastName =
    lastNameOption
    |> Option.map ((+) " ")
    |> Option.defaultValue ""

  f "Добро пожаловать в InsightClub.Creator.Bot, %s%s! \
    С помощью этого бота Вы можете конструировать свои курсы!

    Отправьте /help для получения помощи." firstName lastName
  |> c

let courseStarted =
  c "Создаём новый курс.

    Отправьте текст для того, чтоб задать название."

let courseCanceled = "Создание курса отменено."

let titleReserved = "Название уже занято. Введите другое."

let exitedEditing = "Редактирование завершено."

let error = "Неизвестная команда."

let editingCourse = "Выберите действие."
