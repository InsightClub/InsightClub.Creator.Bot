namespace InsightClub.Creator.Bot

open Microsoft.EntityFrameworkCore.Design


type internal ContextFactory() =
  interface IDesignTimeDbContextFactory<Context> with
    member _.CreateDbContext(_: string array) =
      // Npgsql.EntityFrameworkCore.PostgreSQL requires
      // non-empty connection string.
      new Context("stub")
