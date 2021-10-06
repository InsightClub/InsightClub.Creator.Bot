module InsightClub.Creator.Bot.Api

open Core
open System
open Funogram
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types


// Api helpers
let getUser ctx =
  match ctx.Update with
  | { Message = Some { From = Some user } } ->
    Some user

  | { CallbackQuery = Some { From = user } } ->
    Some user

  | _ ->
    None

let inlineMarkup =
  Option.map
    ( fun markup ->
        { InlineKeyboard = List.map Seq.ofList markup } )

let markup =
  inlineMarkup
  >> Option.map InlineKeyboardMarkup

let removeLastMarkupMaybe config lastId userId =
  async
    { if Option.isSome lastId then
        do!
          Api.editMessageReplyMarkupBase
            (Some <| Int userId) lastId None None
          |> Api.api config
          |> Async.Ignore }

let sendMessage config lastId userId text keyboard  =
  if not <| String.IsNullOrEmpty text then
    Api.sendMessageBase
      (Int userId) text None None None None (markup keyboard)
    |> Api.api config
    |> Async.map
      ( fun r ->
          match r with
          | Ok m ->
            m.ReplyMarkup
            |> Option.map (always m.MessageId)

          | Error _ -> None )
  else
    Async.singleton lastId

let answerCallbackQuery config (query: CallbackQuery) text =
  Api.answerCallbackQueryBase
    (Some query.Id) text None None None
  |> Api.api config
  |> Async.Ignore

let editMessage config lastId userId text keyboard =
  let id = Some <| Int userId

  if not <| String.IsNullOrEmpty text then
    Api.editMessageTextBase
      id lastId None text None None (inlineMarkup keyboard)
    |> Api.api config
    |> Async.Ignore
  else
    Async.doNothing

// Render the state
let handleState (ctx: UpdateContext) connection creatorId lastId state = async {
  // onUpdate must ensure user is available, so this call is safe
  let user = Option.get <| getUser ctx

  let! message, keyboard =
    Render.state (Repo.getCourses connection creatorId) user state

  match ctx.Update with
  | { Message = Some _ } ->
    let! _ =
      removeLastMarkupMaybe ctx.Config lastId user.Id
      |> Async.StartChild

    return!
      sendMessage ctx.Config lastId user.Id message keyboard

  | { CallbackQuery = Some query } ->
    let! _ =
      answerCallbackQuery ctx.Config query None
      |> Async.StartChild

    if lastId.IsSome then
      let! _ =
        editMessage ctx.Config lastId user.Id message keyboard

      return
        keyboard
        |> Option.bind (always lastId)
    else
      return!
        sendMessage ctx.Config lastId user.Id message keyboard

  | _ ->
    return lastId }

// Response for effect
let handleEffect (ctx: UpdateContext) lastId =
  // onUpdate must ensure user is available, so this call is safe
  let user = Option.get <| getUser ctx
  let config = ctx.Config

  function
  | Nothing ->
    Async.singleton lastId

  | ShowDesc text -> async {
    let! _ =
      removeLastMarkupMaybe config lastId user.Id
      |> Async.StartChild

    let text =
      if String.IsNullOrEmpty text
      then $"У Вашего курса пока нет описания {Render.randomEmoji ()}"
      else text

    do!
      Api.sendMessage user.Id text
      |> Api.api config
      |> Async.Ignore

    return None }

  | InformNoPrev -> async {
    match ctx.Update.CallbackQuery with
    | Some query ->
      let! _ =
        answerCallbackQuery ctx.Config query (Some "Вы дошли до минимума")
        |> Async.StartChild

      return lastId

    | None ->
      return lastId }

  | InformNoNext -> async {
    match ctx.Update.CallbackQuery with
    | Some query ->
      let! _ =
        answerCallbackQuery ctx.Config query (Some "Вы дошли до максимума")
        |> Async.StartChild

      return lastId

    | None ->
      return lastId }

// Main function
let onUpdate getConnection ctx =
  let update (user: User) = async {
    use connection = getConnection ()
    let! creatorId, lastId, state = State.get connection user.Id
    let services = Services.get connection creatorId
    let commands = Commands.get ctx
    let! (state, effect) = update services commands state
    let! lastId = handleEffect ctx lastId effect
    let! lastId = handleState ctx connection creatorId lastId state
    do! State.update connection creatorId lastId state }

  ctx
  |> getUser // Ensure user is available
  |> Option.map update
  |> Option.defaultValue Async.doNothing
