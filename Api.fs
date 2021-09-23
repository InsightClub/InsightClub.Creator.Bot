module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open FsToolkit.ErrorHandling
open Microsoft.FSharpLu.Json


module Json = Compact.Strict

let tryGetTelegramUser ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

let updateArrived botConfig getConnection upContext =
  Async.singleton ()
  |> Async.Ignore
