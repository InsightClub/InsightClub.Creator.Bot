namespace InsightClub.Creator.Bot.Model

open InsightClub.Creator.Bot


type internal BlockType =
  | Text = 0
  | Voice = 1

[<CLIMutable>]
type internal Block =
  { CourseId: int
    BlockIndex: int
    BlockType: BlockType
    Content: string }

[<CLIMutable>]
type internal Course =
  { CourseId: int
    CreatorId: int
    CourseName: string
    CourseDescription: string }

[<CLIMutable>]
type internal Creator =
  { CreatorId: int
    TelegramId: int64
    BotState: Core.BotState }
