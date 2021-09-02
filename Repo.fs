module InsightClub.Creator.Bot.Repo

open EntityFrameworkCore.FSharp.DbContextHelpers
open FsToolkit.ErrorHandling
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

    async
      { do! addEntityAsync ctx creator
        do! saveChangesAsync ctx
        return creator }

  async
    { let! creatorOption =
        tryFilterFirstAsync
          <@ fun c -> c.TelegramId = telegramId @>
          ctx.Creators

      if Option.isSome creatorOption
      then return Option.get creatorOption
      else return! createOp }

let updateCreatorAsync (ctx: Context) creator =
  async
    { let! _ = updateEntityAsync ctx (fun c -> c.CreatorId :> obj) creator
      do! saveChangesAsync ctx }

