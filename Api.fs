module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Utils
open Db


let onStart ctx =
  option {
    let! message = ctx.Update.Message
    let! firstName = message.Chat.FirstName
    let! lastName = message.Chat.LastName

    $"Добро пожаловать, {firstName} {lastName}!\n"
    |> sendMessage message.Chat.Id
    |> api ctx.Config
    |> Async.Ignore
    |> Async.Start
  } |> ignore

let updateArrived ctx =
  option {
    let! message = ctx.Update.Message
    let! username = message.Chat.Username

    if Config.Bot.AllowedUsers.Contains(username) then
      processCommands ctx [
        cmd "/start" onStart
        cmd "/restart" onStart
      ]
      |> ignore
    else
      "Нет доступа"
      |> sendMessage message.Chat.Id
      |> api ctx.Config
      |> Async.Ignore
      |> Async.Start
  }   |> ignore
