module Insight.Creator.Bot.Utils

type OptionBuilder() =
  member _.Bind(x, f) = Option.bind f x
  member _.Return(x) = Some x
  member _.ReturnFrom(x) = x
  member _.Zero () = Some ()

let option = OptionBuilder()
