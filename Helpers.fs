module InsightClub.Creator.Bot.Helpers


module Option =
  let onNone f =
    function
    | Some x -> Some x
    | None -> f () |> ignore; None

module Async =
  let pair x y =
    async
      { return x, y }
