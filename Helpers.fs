[<AutoOpen>]
module InsightClub.Creator.Bot.Helpers

open System


/// Returns always first value
let always x _ = x

/// Pairs two values in one tuple
let (&>) x y = (x, y)

let (|Command|_|) command (s: string) =
  if s.StartsWith "/" && s = command then Some () else None

let (|CommandParam|_|) command (s: string) =
  let s = s.Split(" ")
  let ok =
    Array.length s = 2
    && s.[0].StartsWith "/"
    && s.[0] = command

  if ok then
    let ok, res =
      Int32.TryParse s.[1]

    if ok then
      Some res
    else
      None
  else
    None

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
