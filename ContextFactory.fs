module InsightClub.Creator.Bot.ContextFactory

open Microsoft.EntityFrameworkCore.Design
open Context
open Config

type ContextFactory() =
  interface IDesignTimeDbContextFactory<Context> with
    member _.CreateDbContext(_: string array) =
      let filePath = "Config.yaml"

      let fail () =
        failwith $"Please, provide a {filePath} file"

      filePath
      |> Config.tryLoad
      |> Option.map (Config.connStr >> Context.create)
      |> Option.defaultWith fail
