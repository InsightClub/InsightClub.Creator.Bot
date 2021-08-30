module rec InsightClub.Creator.Bot.Model

open System.ComponentModel.DataAnnotations
open System.ComponentModel.DataAnnotations.Schema
open Microsoft.EntityFrameworkCore


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
[<Index("BlockTypeName", IsUnique = true)>]
type BlockType =
  { BlockTypeId: BlockTypeId
    [<Required>]
    BlockTypeName: string }

[<CLIMutable>]
type Block =
  { [<Key>]
    [<Column(Order = 1)>]
    CourseId: int
    [<Key>]
    [<Column(Order = 2)>]
    BlockIndex: int
    BlockTypeId: BlockTypeId
    BlockType: BlockType
    [<Required>]
    Content: string
    Course: Course }

[<CLIMutable>]
[<Index("CreatorId", "CourseName", IsUnique = true)>]
type Course =
  { CourseId: int
    CreatorId: int
    [<Required>]
    CourseName: string
    [<Required>]
    CourseDescription: string
    Creator: Creator
    Blocks: Block list }

[<CLIMutable>]
[<Index("TelegramId", IsUnique = true)>]
type Creator =
  { CreatorId: int
    TelegramId: int64
    Courses: Course list }
