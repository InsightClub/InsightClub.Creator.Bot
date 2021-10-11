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

let removeKeyboard config userId messageId = async {
  do!
    Api.editMessageReplyMarkupBase
      (Some <| Int userId) (Some messageId) None None
    |> Api.api config
    |> Async.StartChild
    |> Async.Ignore }

let sendMessage config userId text keyboard = async {
  return!
    Api.sendMessageBase
      (Int userId) text None None None None (markup keyboard)
    |> Api.api config
    |> Async.map
      ( function
        | Ok m    -> Option.map (always m.MessageId) m.ReplyMarkup
        | Error _ -> None ) }

let answerCallbackQuery config queryId text =
  Api.answerCallbackQueryBase
    (Some queryId) text None None None
  |> Api.api config
  |> Async.StartChild
  |> Async.Ignore

let editMessage config messageId userId text keyboard = async {
  let id = Some (Int userId)
  let messageId = Some messageId
  let keyboard = inlineMarkup keyboard
  do!
    Api.editMessageTextBase
      id messageId None text None None keyboard
    |> Api.api config
    |> Async.StartChild
    |> Async.Ignore }

let renderQueryEffect = function
| Some (Commands.ShowDesc desc) ->
  let text =
    if desc = String.Empty
    then $"У Вашего курса пока нет описания {Render.randomEmoji ()}"
    else desc

  Some text, None

| Some Commands.InformMin ->
  None, Some "Вы дошли до минимума"

| Some Commands.InformMax ->
  None, Some "Вы дошли до максимума"

| None ->
  None, None

let onUpdate getConnection ctx = async {
  use connection = getConnection ()
  let config = ctx.Config

  match ctx.Update with
  | { Message = Some ({ From = Some user } as message) } ->
    let! creatorId, lastId, state = State.get connection user.Id

    let services = Services.get connection creatorId
    let commands = Commands.onMessage message
    let! state, _ = Core.update services commands state

    let getCourses = Repo.getCourses connection creatorId
    let! text, keyboard = Render.state getCourses user state

    match lastId with
    | Some messageId ->
      do! removeKeyboard config user.Id messageId

    | None ->
      ()

    let! lastId =
      if text <> String.Empty then
        sendMessage config user.Id text keyboard
      else
        Async.singleton None

    do! State.update connection creatorId lastId state

  | { CallbackQuery = Some ({ From = user; Message = Some message } as query) } ->
    let! creatorId, _, state = State.get connection user.Id

    let services = Services.get connection creatorId
    let commands = Commands.onQuery query
    let! state, effect = Core.update services commands state

    let effectText, queryAnswer = renderQueryEffect effect

    let getCourses = Repo.getCourses connection creatorId
    let! text, keyboard = Render.state getCourses user state

    do! answerCallbackQuery config query.Id queryAnswer

    let! lastId = async {
      match effectText with
      | Some effectText ->
        do! removeKeyboard config user.Id message.MessageId

        do!
          sendMessage config user.Id effectText None
          |> Async.Ignore

        if text <> String.Empty then
          return! sendMessage config user.Id text keyboard
        else
          return None

      | None ->
        if text <> String.Empty then
          do! editMessage config message.MessageId user.Id text keyboard
          return Option.map (always message.MessageId) keyboard
        else
          return Some message.MessageId }

    do! State.update connection creatorId lastId state

  | _ ->
    () }
