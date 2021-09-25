module InsightClub.Creator.Bot.Message

open System.Text.RegularExpressions


// Cleaning
let c s = Regex("\n[ ]+").Replace(s, "\n")
let f = sprintf

// Line
let ln = "\n"
let doubleLn = ln + ln

// Texts
let greeting firstName lastNameOption =
  let lastName =
    lastNameOption
    |> Option.map ((+) " ")
    |> Option.defaultValue ""

  f "Добро пожаловать в InsightClub.Creator.Bot, %s%s! \
    С помощью этого бота Вы можете конструировать свои курсы!

    Отправьте /help для получения помощи." firstName lastName
  |> c

let courseCanceled = "Создание курса отменено."
let error = "Неизвестная команда."

let help =
  c "Этот бот позволяет пошагово создавать свои курсы в Телеграм."

let commands =
  c "/help — Получить помощь
     /new — Начать создавать курс"

let newCourse = "Создаём новый курс. Пришлите имя будущего курса."
