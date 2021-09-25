[<AutoOpen>]
module InsightClub.Creator.Bot.Helpers


module Async =
  let singleton n = async { return n }
  let map f comp =
    async
      { let! r = comp
        return f r }
