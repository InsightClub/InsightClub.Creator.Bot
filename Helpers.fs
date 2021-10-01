[<AutoOpen>]
module InsightClub.Creator.Bot.Helpers


let always x _ = x

let (|Command|_|) command s =
  if (s = command) then Some () else None

let (|PlainText|_|) (s: string) =
  if s.StartsWith "/" then None else Some s

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
