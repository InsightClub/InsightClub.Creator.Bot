module internal rec InsightClub.Creator.Bot.Model

open System.ComponentModel.DataAnnotations
open System.ComponentModel.DataAnnotations.Schema
open Microsoft.EntityFrameworkCore
open Core


// EfCore.FSharp doesn't understand that strings are required by default.
// As in one of it's issues on GitHub was mentioned, it will be fixed after
// EFCore 6 release. So [<Required>] attribute is required on every
// string field by now.

type BlockTypeId =
  | Text = 0
  | Voice = 1

// Separate entity ensures that with code changes existing
// blocks will have the same type.
[<CLIMutable>]
type BlockType =
  { BlockTypeId: BlockTypeId
    BlockTypeName: string }

[<CLIMutable>]
type Block =
  { CourseId: int
    BlockIndex: int
    BlockTypeId: BlockTypeId
    Content: string }

[<CLIMutable>]
type Course =
  { CourseId: int
    CreatorId: int
    CourseName: string
    CourseDescription: string }

[<CLIMutable>]
type Creator =
  { CreatorId: int
    TelegramId: int64
    BotState: BotState }
