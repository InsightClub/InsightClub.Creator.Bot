module InsightClub.Creator.Bot.Repo

open EntityFrameworkCore.FSharp.DbContextHelpers
open Helpers
open Core
open Model
open Context


let getCreatorAsync (ctx: Context) telegramId =
  let createOp =
    let creator =
      { CreatorId = 0
        TelegramId = telegramId
        BotState = initialState }

    creator
    |> addEntityAsync ctx
    |> Async.bind (depute saveChangesAsync ctx)
    |> Async.map (always creator)

  ctx.Creators
  |> tryFilterFirstAsync <@ fun c -> c.TelegramId = telegramId @>
  |> Async.bind
      ( Option.map Async.unit
        >> Option.defaultValue createOp )

let updateCreatorAsync (ctx: Context) creator =
  updateEntityAsync ctx (fun c -> c.CreatorId :> obj) creator
