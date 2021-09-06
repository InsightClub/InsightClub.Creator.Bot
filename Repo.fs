module InsightClub.Creator.Bot.Repo

open Microsoft.EntityFrameworkCore
open EntityFrameworkCore.FSharp.DbContextHelpers
open FsToolkit.ErrorHandling
open Helpers
open Core
open Model
open Context


let getOrAddCreator (ctx: Context) telegramId =
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

let updateCreator (ctx: Context) creator =
  async
    { let! _ = updateEntityAsync ctx (fun c -> c.CreatorId :> obj) creator
      do! saveChangesAsync ctx }

let checkCourseNameReserved (ctx: Context) name =
  ctx.Courses.AnyAsync(fun c -> c.CourseName = name)
  |> Async.AwaitTask
