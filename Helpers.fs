[<AutoOpen>]
module InsightClub.Creator.Bot.Helpers


module Async =
  let singleton n = async { return n }

  let doNothing = singleton ()

  let map f comp =
    async
      { let! r = comp
        return f r }

  let always x comp =
    async
      { let! _ = comp
        return x }
