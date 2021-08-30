namespace InsightClub.Creator.Bot

open Microsoft.EntityFrameworkCore.Design


type internal ContextFactory() =
  interface IDesignTimeDbContextFactory<Context> with
    member _.CreateDbContext(_: string array) =
      let filePath = "Config.yaml"

      let fail () =
        failwith $"Please, provide a {filePath} file"

      filePath
      |> Config.tryLoad
      |> Option.map (fun config -> new Context(config.Db.ConnectionString))
      |> Option.defaultWith fail
