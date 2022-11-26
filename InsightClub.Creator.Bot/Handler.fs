module InsightClub.Creator.Bot.Handler

open System
open Funogram
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types


let inlineMarkup =
  Option.map
    ( fun markup ->
        { InlineKeyboard = markup |> List.toArray |> Array.map List.toArray } )

let markup =
  inlineMarkup
  >> Option.map InlineKeyboardMarkup

let removeKeyboard config userId messageId = async {
  do!
    Req.EditMessageReplyMarkup.Make(Int userId, messageId)
    |> Api.api config
    |> Async.StartChild
    |> Async.Ignore }

let sendMessage config userId text keyboard = async {
  return!
    Req.SendMessage.Make(Int userId, text, ?replyMarkup = markup keyboard)
    |> Api.api config
    |> Async.map
      ( function
        | Ok m    -> Option.map (always m.MessageId) m.ReplyMarkup
        | Error _ -> None ) }

let answerCallbackQuery config queryId text =
  Req.AnswerCallbackQuery.Make(queryId, ?text = text)
  |> Api.api config
  |> Async.StartChild
  |> Async.Ignore

let editMessage config messageId userId text keyboard =
    Req.EditMessageText.Make(
      text,
      chatId = Int userId,
      messageId = messageId,
      ?replyMarkup = inlineMarkup keyboard
    )
    |> Api.api config
    |> Async.StartChild
    |> Async.Ignore

let sendContent config userId storage content = async {
  let getFile fileId = async {
    let! fileStream =
      Storage.getFile storage fileId

    return InputFile.File (fileId, fileStream) }

  match content with
  | Bot.Text text ->
    do!
      Req.SendMessage.Make(Int userId, text)
      |> Api.api config
      |> Async.Ignore

  | Bot.Photo fileId ->
    do!
      Req.SendChatAction.Make(userId, ChatAction.UploadPhoto)
      |> Api.api config
      |> Async.Ignore

    let! file =
      getFile fileId

    do!
      Req.SendPhoto.Make(userId, file)
      |> Api.api config
      |> Async.Ignore

  | Bot.Audio fileId ->
    do!
      Req.SendChatAction.Make(userId, ChatAction.UploadAudio)
      |> Api.api config
      |> Async.Ignore

    let! file =
      getFile fileId

    do!
      Req.SendAudio.Make(userId, file)
      |> Api.api config
      |> Async.Ignore

  | Bot.Video fileId ->
    do!
      Req.SendChatAction.Make(userId, ChatAction.UploadVideo)
      |> Api.api config
      |> Async.Ignore

    let! file =
      getFile fileId

    do!
      Req.SendVideo.Make(userId, file)
      |> Api.api config
      |> Async.Ignore

  | Bot.Voice fileId ->
    do!
      Req.SendChatAction.Make(userId, ChatAction.UploadAudio)
      |> Api.api config
      |> Async.Ignore

    let! file =
      getFile fileId

    do!
      Req.SendVoice.Make(userId, file)
      |> Api.api config
      |> Async.Ignore

  | Bot.Document fileId ->
    do!
      Req.SendChatAction.Make(userId, ChatAction.UploadDocument)
      |> Api.api config
      |> Async.Ignore

    let! file =
      getFile fileId

    do!
      Req.SendDocument.Make(userId, file)
      |> Api.api config
      |> Async.Ignore

  | Bot.VideoNote fileId ->
    do!
      Req.SendChatAction.Make(userId, ChatAction.UploadVideoNote)
      |> Api.api config
      |> Async.Ignore

    let! file =
      getFile fileId

    do!
      Req.SendVideoNote.Make(userId, file)
      |> Api.api config
      |> Async.Ignore }

let getRenderServices connection creatorId : Render.Services =
  { getCourses = Repo.getCourses connection creatorId
    getBlocks = Repo.getBlocks connection
    getCourseTitle = Repo.getCourseTitle connection
    getCourseDesc = Repo.getCourseDesc connection }

let onUpdate connectToDb connectToStorage ctx =
  async {
    use connection = connectToDb ()
    use storage = connectToStorage ()
    let config = ctx.Config

    match ctx.Update with
    // Message updates
    | { Message = Some ({ From = Some user } as message) } ->
      let! creatorId, lastId, state = State.get connection user.Id

      let services = Services.get connection config storage creatorId

      let dispatcher = Dispatcher.dispatchMessage message

      let return' = Async.singleton >> always

      let! state = Bot.update return' dispatcher services state

      let renderServices = getRenderServices connection creatorId

      let! text, keyboard = Render.state renderServices user state

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

      let services = Services.get connection config storage creatorId

      let dispatcher = Dispatcher.dispatchCallbackQuery query

      let return' state effect = Async.singleton (state, effect)

      let! state, effect = Bot.update return' dispatcher services state

      let effectContents, queryAnswer = Render.queryEffect effect

      let renderServices = getRenderServices connection creatorId

      let! text, keyboard = Render.state renderServices user state

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
            |> List.map (sendContent config user.Id storage)
            |> Async.Sequential
            |> Async.Ignore

          if text <> String.Empty then
            return! sendMessage config user.Id text keyboard

          else
            return None }

      do! State.update connection creatorId lastId state

    | _ ->
      ()
  }
  |> Async.RunSynchronously
