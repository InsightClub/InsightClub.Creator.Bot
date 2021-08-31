module InsightClub.Creator.Bot.Context.Factory

open Microsoft.EntityFrameworkCore.Design
open InsightClub.Creator.Bot

type ContextFactory() =
  interface IDesignTimeDbContextFactory<Context> with
    member _.CreateDbContext(_: string array) =
      let filePath = "Config.yaml"

      let fail () =
        failwith $"Please, provide a {filePath} file"

      filePath
      |> Config.tryLoad
      |> Option.map (fun c -> new Context(Config.getConnectionString c))
      |> Option.defaultWith fail
