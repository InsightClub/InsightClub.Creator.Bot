module InsightClub.Creator.Bot.Utils

type OptionBuilder() =
  member _.Bind(x, f) = Option.bind f x
  member _.Return(x) = Some x
  member _.ReturnFrom(x) = x
  member _.Zero () = Some ()
  member _.Combine (a, b) =
    match a, b with
    | Some a', Some b' -> Some b'
    | Some a', None -> Some a'
    | None, Some b' -> Some b'
    | None, None -> None
  member _.Delay(f) = f()

let option = OptionBuilder()
