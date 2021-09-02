module InsightClub.Creator.Bot.Helpers


module Option =
  let onNone f =
    function
    | Some x -> Some x
    | None -> f () |> ignore; None
