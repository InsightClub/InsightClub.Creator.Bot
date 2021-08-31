namespace InsightClub.Creator.Bot.Model.Design

open Microsoft.EntityFrameworkCore.Design
open InsightClub.Creator.Bot
open InsightClub.Creator.Bot.Model

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
