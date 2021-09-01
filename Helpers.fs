module InsightClub.Creator.Bot.Helpers


let always x _ = x
let depute f x _ = f x
let deputeAction f = depute f ()

module Async =
  let bind f asyncOp = async.Bind(asyncOp, f)
  let unit value = async.Return value
  let map f asyncOp = bind (f >> unit) asyncOp
