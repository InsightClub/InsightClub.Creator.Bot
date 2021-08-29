namespace InsightClub.Creator.Bot

open System
open System.Linq
open Microsoft.EntityFrameworkCore
open EntityFrameworkCore.FSharp.Extensions
open Model


type Context(connectionString: string) =
  inherit DbContext()

  member this.BlockTypes = this.Set<BlockType>()
  member this.Blocks = this.Set<Block>()
  member this.Courses = this.Set<Course>()
  member this.Creators = this.Set<Creator>()

  override _.OnModelCreating builder =
    builder.RegisterOptionTypes()

    builder
      .Entity<BlockType>()
      .Property(fun t -> t.BlockTypeId)
      .HasConversion<int>()
    |> ignore

    builder
      .Entity<BlockType>()
      .HasData(
        (query {
          for t in Enum.GetValues<BlockTypeId>() do
            select { BlockTypeId = t; BlockTypeName = string t }
         })
          .ToArray()
      )
    |> ignore

    builder
      .Entity<Block>()
      .Property(fun b -> b.BlockTypeId)
      .HasConversion<int>()
    |> ignore


  override _.OnConfiguring(options: DbContextOptionsBuilder) =
    options
      .UseNpgsql(connectionString)
      .UseFSharpTypes()
    |> ignore
