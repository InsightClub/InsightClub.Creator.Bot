module InsightClub.Creator.Bot.Api

open System
open Funogram
open Funogram.Types
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

let sendContent userId storagePath =
  let makeFile fileId =
    FileToSend.File <| (fileId, Storage.getFile storagePath fileId)

  function
  | Core.Text text ->
    Api.sendMessage userId text
    :> IRequestBase<Message>

  | Core.Photo fileId ->
    Api.sendPhoto userId (makeFile fileId) ""
    :> IRequestBase<Message>

  | Core.Audio fileId ->
    Api.sendAudio userId (makeFile fileId) "" None None
    :> IRequestBase<Message>

  | Core.Video fileId ->
    Api.sendVideo userId (makeFile fileId) ""
    :> IRequestBase<Message>

  | Core.Voice fileId ->
    Api.sendVoice userId (makeFile fileId) ""
    :> IRequestBase<Message>

  | Core.Document fileId ->
    Api.sendDocument userId (makeFile fileId) ""
    :> IRequestBase<Message>

  | Core.VideoNote fileId ->
    Api.sendVideoNote userId (makeFile fileId)
    :> IRequestBase<Message>

let onUpdate getConnection storagePath ctx = async {
  use connection = getConnection ()
  let config = ctx.Config

  match ctx.Update with
  // Message updates
  | { Message = Some ({ From = Some user } as message) } ->
    let! creatorId, lastId, state = State.get connection user.Id

    let services = Services.get connection config storagePath creatorId
    let commands = Commands.onMessage message
    let! state, _ = Core.update services commands state

    let getCourses = Repo.getCourses connection creatorId
    let getBlocks = Repo.getBlocks connection
    let! text, keyboard = Render.state getCourses getBlocks user state

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

  // Inline keyboard updates
  | { CallbackQuery = Some ({ From = user; Message = Some message } as query) } ->
    let! creatorId, _, state = State.get connection user.Id

    let services = Services.get connection config storagePath creatorId
    let commands = Commands.onQuery query
    let! state, effect = Core.update services commands state

    let effectContents, queryAnswer = Render.queryEffect effect

    let getCourses = Repo.getCourses connection creatorId
    let getBlocks = Repo.getBlocks connection
    let! text, keyboard = Render.state getCourses getBlocks user state

    do! answerCallbackQuery config query.Id queryAnswer

    let! lastId = async {
      match effectContents with
      | [ ] ->
        if text <> String.Empty then
          do! editMessage config message.MessageId user.Id text keyboard
          return Option.map (always message.MessageId) keyboard
        else
          return Some message.MessageId

      | contents ->
        do! removeKeyboard config user.Id message.MessageId

        do!
          contents
          |> List.map (sendContent user.Id storagePath >> Api.api config)
          |> Async.Sequential
          |> Async.Ignore

        if text <> String.Empty then
          return! sendMessage config user.Id text keyboard
        else
          return None }

    do! State.update connection creatorId lastId state

  | _ ->
    () }
