module InsightClub.Creator.Bot.Repo

open System
open InsightClub.Db
open Microsoft.EntityFrameworkCore
open EntityFrameworkCore.FSharp.DbContextHelpers
open FsToolkit.ErrorHandling


let getOrAddCreator (ctx: BotContext) telegramId =
  let createOp =
    let creator =
      { CreatorId = Guid.NewGuid ()
        TelegramId = telegramId
        TelegramBotState = "" }

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

let updateCreator (ctx: BotContext) creator =
  async
    { let! _ = updateEntityAsync ctx (fun c -> c.CreatorId :> obj) creator
      do! saveChangesAsync ctx }

let checkCourseNameReserved (ctx: BotContext) name =
  ctx.Courses.AnyAsync(fun c -> c.CourseName = name)
  |> Async.AwaitTask

let addCourse (ctx: BotContext) (course: Course) (blocks: Content list) =
  async
    { do! addEntityAsync ctx course

      let blocks =
        blocks
        |> List.map
            ( fun b ->
                { b with CourseId = course.CourseId } )

      do! addEntityRangeAsync ctx blocks
      do! saveChangesAsync ctx }
