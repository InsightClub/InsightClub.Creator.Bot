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

let answerMessage config userId message keyboard lastId = async {
  let! _ =
    removeLastMarkupMaybe config userId lastId
    |> Async.StartChild

  return!
    sendMessage config userId message keyboard lastId }

let answerQuery config userId message keyboard lastId query = async {
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

let handleEffect config userId lastId effect query = async {
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

let getData = function
| { Update.Message = Some ({ From = Some user } as message) } ->
  Some (user, Choice1Of2 message)

| { CallbackQuery = Some ({ From = user; Message = Some _ } as query) } ->
  Some (user, Choice2Of2 query)

| _ ->
  None

let choice onMessage onQuery = function
| Choice1Of2 message -> onMessage message
| Choice2Of2 query   -> onQuery query

let onUpdate getConnection ctx = async {
  match getData ctx.Update with
  | Some (user, req) ->
    let config = ctx.Config
    let userId = user.Id
    let commands = choice Commands.onMessage Commands.onQuery req

    use connection = getConnection ()
    let! creatorId, lastId, state = State.get connection userId
    let services = Services.get connection creatorId

    let! state, effect = Core.update services commands state

    let messageEffect _ = Async.singleton lastId
    let queryEffect = handleEffect config userId lastId effect

    let! lastId = choice messageEffect queryEffect req

    let getCourses = Repo.getCourses connection creatorId
    let! message, keyboard = Render.state getCourses user state

    let answerMessage _ = answerMessage config userId message keyboard lastId
    let answerQuery = answerQuery config userId message keyboard lastId

    let! lastId = choice answerMessage answerQuery req

    do! State.update connection creatorId lastId state

  | _ ->
    () }
