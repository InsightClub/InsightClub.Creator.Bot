namespace InsightClub.Creator.Bot

open Microsoft.EntityFrameworkCore.Design


type internal ContextFactory() =
  interface IDesignTimeDbContextFactory<Context> with
    member _.CreateDbContext(_: string array) =
      new Context("")
