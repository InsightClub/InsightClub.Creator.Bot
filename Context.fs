namespace InsightClub.Creator.Bot

open System
open Microsoft.EntityFrameworkCore
open EntityFrameworkCore.FSharp.Extensions
open Model


type internal Context(connectionString: string) =
  inherit DbContext()

  member this.BlockTypes = this.Set<BlockType>()
  member this.Blocks = this.Set<Block>()
  member this.Courses = this.Set<Course>()
  member this.Creators = this.Set<Creator>()

  override _.OnModelCreating builder =
    builder.RegisterOptionTypes()

    // BlockType
    builder
      .Entity<BlockType>()
      .Property(fun t -> t.BlockTypeId)
      .HasConversion<int>()
    |> ignore

    builder
      .Entity<BlockType>()
      .Property(fun t -> t.BlockTypeName)
      .IsRequired()
    |> ignore

    builder
      .Entity<BlockType>()
      .HasKey(fun t -> t.BlockTypeId :> obj)
    |> ignore

    builder
      .Entity<BlockType>()
      .HasIndex(fun t -> t.BlockTypeName :> obj)
      .IsUnique()
    |> ignore

    let getBlockType i =
      { BlockTypeId = i
        BlockTypeName = string i }

    builder
      .Entity<BlockType>()
      .HasData(
        Enum.GetValues<BlockTypeId>()
        |> Array.map getBlockType
      )
    |> ignore

    // Block
    builder
      .Entity<Block>()
      .Property(fun b -> b.BlockTypeId)
      .HasConversion<int>()
    |> ignore

    builder
      .Entity<Block>()
      .Property(fun b -> b.Content)
      .IsRequired()
      .IsUnicode()
    |> ignore

    builder
      .Entity<Block>()
      .HasKey(
        fun b -> (b.CourseId, b.BlockIndex) :> obj)
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
      .HasIndex(
        fun c -> (c.CreatorId, c.CourseName) :> obj)
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
      .HasKey(fun c -> c.CreatorId :> obj)
    |> ignore

    builder
      .Entity<Creator>()
      .HasIndex(fun c -> c.TelegramId :> obj)
      .IsUnique()
    |> ignore

    builder
      .Entity<Creator>()
      .Property(fun c -> c.BotState)
      .HasColumnType("jsonb")
    |> ignore

  override _.OnConfiguring(options: DbContextOptionsBuilder) =
    options
      .UseNpgsql(connectionString)
      .UseFSharpTypes()
    |> ignore
