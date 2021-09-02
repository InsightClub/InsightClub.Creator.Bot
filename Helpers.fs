module InsightClub.Creator.Bot.Helpers


let always x _ = x
let depute f x _ = f x
let deputeAction f = depute f ()

let tryCatch onSuccess onException f x =
  try f x |> onSuccess
  with e -> onException e

module Async =
  let bind f asyncOp = async.Bind(asyncOp, f)
  let unit value = async.Return value
  let map f asyncOp = bind (f >> unit) asyncOp

module Option =
  let tryCatch f x =
    tryCatch Some (always None) f x

  let onNone f =
    function
    | Some x -> Some x
    | None -> f () |> ignore; None
