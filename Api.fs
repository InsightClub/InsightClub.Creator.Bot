module InsightClub.Creator.Bot.Api

open System
open Funogram
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types


let inlineMarkup =
  Option.map
    ( fun markup ->
        { InlineKeyboard = List.map Seq.ofList markup } )

let markup =
  inlineMarkup
  >> Option.map InlineKeyboardMarkup

let removeLastMarkupMaybe config userId lastId = async {
  if Option.isSome lastId then
    do!
      Api.editMessageReplyMarkupBase
        (Some <| Int userId) lastId None None
      |> Api.api config
      |> Async.Ignore }

let sendMessage config userId text keyboard lastId = async {
  if not <| String.IsNullOrEmpty text then
    return!
      Api.sendMessageBase
        (Int userId) text None None None None (markup keyboard)
      |> Api.api config
      |> Async.map
        ( function
          | Ok m ->
            m.ReplyMarkup
            |> Option.map (always m.MessageId)

          | Error _ -> None )
  else
    return lastId }

let answerCallbackQuery config (query: CallbackQuery) text =
  Api.answerCallbackQueryBase
    (Some query.Id) text None None None
  |> Api.api config
  |> Async.Ignore

let editMessage config lastId userId text keyboard = async {
  let id = Some <| Int userId

  if not <| String.IsNullOrEmpty text then
    do!
      Api.editMessageTextBase
        id lastId None text None None (inlineMarkup keyboard)
      |> Api.api config
      |> Async.Ignore
  else
    () }

let handleMessage config userId message keyboard lastId = async {
  let! _ =
    removeLastMarkupMaybe config userId lastId
    |> Async.StartChild

  return!
    sendMessage config userId message keyboard lastId }

let handleQuery config userId query message keyboard lastId = async {
  let! _ =
    answerCallbackQuery config query None
    |> Async.StartChild

  if Option.isSome lastId then
    let! _ =
      editMessage config lastId userId message keyboard

    return
      keyboard
      |> Option.bind (always lastId)
  else
    return!
      sendMessage config userId message keyboard lastId }

let handleEffect config userId query lastId effect = async {
  match effect with
  | Core.Nothing ->
    return lastId

  | Core.ShowDesc text ->
    let! _ =
      removeLastMarkupMaybe config userId lastId
      |> Async.StartChild

    let text =
      if String.IsNullOrEmpty text
      then $"У Вашего курса пока нет описания {Render.randomEmoji ()}"
      else text

    do!
      Api.sendMessage userId text
      |> Api.api config
      |> Async.Ignore

    return None

  | Core.InformNoPrev ->
    let! _ =
      answerCallbackQuery config query (Some "Вы дошли до минимума")
      |> Async.StartChild

    return lastId

  | Core.InformNoNext ->
    let! _ =
      answerCallbackQuery config query (Some "Вы дошли до максимума")
      |> Async.StartChild

    return lastId }

// Main function
let onUpdate getConnection ctx = async {
  let config = ctx.Config
  match ctx.Update with
  | { Message = Some ({ From = Some user } as message) } ->
    let userId = user.Id
    let commands = Commands.onMessage message

    use connection = getConnection ()
    let! creatorId, lastId, state = State.get connection userId
    let services = Services.get connection creatorId
    let getCourses = Repo.getCourses connection creatorId

    let! state, _ = Core.update services commands state

    let! message, keyboard = Render.state getCourses user state
    let! lastId = handleMessage config userId message keyboard lastId

    do! State.update connection creatorId lastId state

  | { CallbackQuery = Some ({ From = user; Message = Some _ } as query) } ->
    let userId = user.Id
    let commands = Commands.onQuery query

    use connection = getConnection ()
    let! creatorId, lastId, state = State.get connection userId
    let services = Services.get connection creatorId
    let getCourses = Repo.getCourses connection creatorId

    let! state, effect = Core.update services commands state

    let! message, keyboard = Render.state getCourses user state
    let! lastId = handleEffect config userId query lastId effect
    let! lastId = handleQuery config userId query message keyboard lastId

    do! State.update connection creatorId lastId state

  | _ ->
    () }
