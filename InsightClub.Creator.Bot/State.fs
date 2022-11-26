module InsightClub.Creator.Bot.State

open Microsoft.FSharpLu.Json

module Json = Compact.Strict


type State = {
  /// Id of the last message sent with the inline keyboard
  LastId: int64 option
  State: Bot.State
}

let create lastId state =
  { LastId = lastId
    State = state }

let initialJson =
  create None Bot.initialState
  |> Json.serialize

let get connection telegramId =
  Repo.getState initialJson connection telegramId
  |> Async.map
    ( fun (creatorId, stateJson) ->
        let { LastId = lastId; State = state } =
          Json.deserialize<State> stateJson

        creatorId, lastId, state )

let update connection creatorId lastId state =
  Repo.updateState
    connection creatorId (create lastId state |> Json.serialize)
