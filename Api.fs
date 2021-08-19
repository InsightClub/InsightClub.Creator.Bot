module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Utils


let onStart ctx =
  option {
    let! message = ctx.Update.Message
    let! name = message.Chat.FirstName

    sprintf "Hello, %s!" name
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
