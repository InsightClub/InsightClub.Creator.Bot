module internal rec InsightClub.Creator.Bot.Model


type BlockType =
  | Text = 0
  | Voice = 1

[<CLIMutable>]
type Block =
  { CourseId: int
    BlockIndex: int
    BlockType: BlockType
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
    BotState: Core.BotState }
