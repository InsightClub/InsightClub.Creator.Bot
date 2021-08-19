module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Utils


let onStart ctx =
  option {
    let! message = ctx.Update.Message
    let! firstName = message.Chat.FirstName
    let! lastName = message.Chat.LastName

    $"Добро пожаловать, {firstName} {lastName}!\n"
    + "Введите /help, чтоб получить информацию и список команд."
    |> sendMessage message.Chat.Id
    |> api ctx.Config
    |> Async.Ignore
    |> Async.Start
  } |> ignore

let updateArrived ctx =
  processCommands ctx [
    cmd "/start" onStart
  ]
  |> ignore
