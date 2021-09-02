module InsightClub.Creator.Bot.Context

open Microsoft.FSharpLu.Json
open Microsoft.EntityFrameworkCore
open EntityFrameworkCore.FSharp.Extensions
open Model


module Json = Compact.Strict

type Context(connectionString: string) =
  inherit DbContext()

  member this.Blocks = this.Set<Block>()
  member this.Courses = this.Set<Course>()
  member this.Creators = this.Set<Creator>()

  override _.OnModelCreating builder =
    builder.RegisterOptionTypes()

    // Block
    builder
      .Entity<Block>()
      .Property(fun b -> b.BlockType)
      .HasConversion<string>()
      .IsRequired()
    |> ignore

    builder
      .Entity<Block>()
      .Property(fun b -> b.Content)
      .IsRequired()
      .IsUnicode()
    |> ignore

    builder
      .Entity<Block>()
      .HasKey(fun b -> (b.CourseId, b.BlockIndex) :> obj)
      |> ignore

    builder
      .Entity<Block>()
      .HasOne<Course>()
      .WithMany()
      .HasForeignKey(fun b -> b.CourseId :> obj)
    |> ignore

    // Course
    builder
      .Entity<Course>()
      .Property(fun c -> c.CourseName)
      .IsRequired()
      .IsUnicode()
    |> ignore

    builder
      .Entity<Course>()
      .Property(fun c -> c.CourseDescription)
      .IsRequired()
      .IsUnicode()
    |> ignore

    builder
      .Entity<Course>()
      .HasKey(fun c -> c.CourseId :> obj)
    |> ignore

    builder
      .Entity<Course>()
      .HasIndex(fun c -> (c.CreatorId, c.CourseName) :> obj)
      .IsUnique()
    |> ignore

    builder
      .Entity<Course>()
      .HasOne<Creator>()
      .WithMany()
      .HasForeignKey(fun c -> c.CreatorId :> obj)
    |> ignore

    // Creator
    builder
      .Entity<Creator>()
      .Property(fun c -> c.BotState)
      .HasConversion(Json.serialize, Json.deserialize)
      .HasColumnType("jsonb")
      .IsRequired()
    |> ignore

    builder
      .Entity<Creator>()
      .HasKey(fun c -> c.CreatorId :> obj)
    |> ignore

    builder
      .Entity<Creator>()
      .HasIndex(fun c -> c.TelegramId :> obj)
      .IsUnique()
    |> ignore

  override _.OnConfiguring(options: DbContextOptionsBuilder) =
    options
      .UseNpgsql(connectionString)
      .UseFSharpTypes()
    |> ignore

module Context =
  let create connectionString =
    new Context(connectionString)

  let canConnect (context: Context) =
    try context.Database.CanConnect()
    with _ -> false
