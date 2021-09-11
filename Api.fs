module InsightClub.Creator.Bot.Api

open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open FsToolkit.ErrorHandling
open Core
open Model
open Context
open Repo


let tryGetTelegramUser ctx =
  ctx.Update.Message
  |> Option.bind (fun m -> m.From)

let getEvent ctx =
  match ctx.Update.Message with
  | Some { Text = Some text } ->
    match text with
    | "/start"  -> BotEvent.CommandReceived BotCommand.Start
    | "/help"   -> BotEvent.CommandReceived BotCommand.Help
    | "/new"    -> BotEvent.CommandReceived BotCommand.New
    | "/undo"   -> BotEvent.CommandReceived BotCommand.Undo
    | "/skip"   -> BotEvent.CommandReceived BotCommand.Skip
    | "/cancel" -> BotEvent.CommandReceived BotCommand.Cancel
    | "/finish" -> BotEvent.CommandReceived BotCommand.Finish
    | _         -> BotEvent.DataReceived <| BotData.Text text

  | Some { Voice = Some voice } ->
    BotEvent.DataReceived <| BotData.Voice voice.FileId

  | _ ->
    BotEvent.UnsupportedReceived

let sendMessage config chatId =
  sendMessage chatId
  >> api config
  >> Async.Ignore

let showHelp config chatId =
  let showHelp help commands =
    Message.help help commands
    |> sendMessage config chatId

  function
  | Help.Idle ->
    showHelp Message.Idle.help Message.Idle.commands

  | Help.PendingName ->
    showHelp Message.PendingName.help Message.PendingName.commands

  | Help.PendingDesc ->
    showHelp Message.PendingDesc.help Message.PendingDesc.commands

  | Help.PendingData ->
    showHelp Message.PendingData.help Message.PendingData.commands

let reportUnsupported config chatId =
  let reportUnsupported commands =
    Message.unsupported commands
    |> sendMessage config chatId

  function
  | Help.Idle ->
    reportUnsupported Message.Idle.commands

  | Help.PendingName ->
    reportUnsupported Message.PendingName.commands

  | Help.PendingDesc ->
    reportUnsupported Message.PendingDesc.commands

  | Help.PendingData ->
    reportUnsupported Message.PendingData.commands

let handleIntent config ctx chatId name creatorId =
  let sendMessage = sendMessage config chatId

  function
  | BotIntent.Ignore ->
    Async.singleton () // Do nothing

  | BotIntent.ShowHelp help ->
    showHelp config chatId help

  | BotIntent.ReportUnsupported help->
    reportUnsupported config chatId help

  | BotIntent.ReportStarted ->
    let firstName, lastNameOption = name
    Message.Inactive.greet firstName lastNameOption
    |> sendMessage

  | BotIntent.ReportCourseStarted ->
    sendMessage Message.Idle.newCourse

  | BotIntent.ReportCourseCanceled ->
    sendMessage Message.cancel

  | BotIntent.ReportNameReserved ->
    sendMessage Message.PendingName.reserved

  | BotIntent.ReportNameSet ->
    sendMessage Message.PendingName.set

  | BotIntent.ReportNameUndone ->
    sendMessage Message.PendingDesc.undo

  | BotIntent.ReportDescSkipped ->
    sendMessage Message.PendingDesc.skip

  | BotIntent.ReportDescSet ->
    sendMessage Message.PendingDesc.set

  | BotIntent.ReportDescUndone ->
    sendMessage Message.PendingData.undoEmpty

  | BotIntent.ReportDataUndone count ->
    sendMessage (Message.PendingData.undo <| count + 1)

  | BotIntent.ReportDataTextSet count ->
    sendMessage (Message.PendingData.textSet count)

  | BotIntent.ReportDataVoiceSet count ->
    sendMessage (Message.PendingData.voiceSet count)

  | BotIntent.SaveCourse acc ->
    let course =
      { CourseId = 0
        CreatorId = creatorId
        CourseName = acc.Name
        CourseDescription = acc.Desc }

    let mapi i b =
      let blockType, content =
        match b with
        | BotData.Text text ->
          (BlockType.Text, text)
        | BotData.Voice filePath ->
          (BlockType.Voice, filePath)

      { CourseId = 0
        BlockIndex = i
        BlockType = blockType
        Content = content }

    let blocks =
      acc.Blocks
      |> List.rev
      |> List.mapi mapi

    async
      { do! addCourse ctx course blocks
        do! sendMessage Message.PendingData.finish }

let createServices ctx =
  { checkNameReserved =
    fun name answer ->
      async
        { let! reserved = checkCourseNameReserved ctx name
          return! answer reserved } }

let updateArrived botConfig (getContext: unit -> Context) upContext =
  asyncOption
    { use dbContext = getContext ()

      let! user = tryGetTelegramUser upContext
      let chatId = user.Id
      let name = user.FirstName, user.LastName

      let! creator = getOrAddCreator dbContext user.Id
      let creatorId = creator.CreatorId

      let event = getEvent upContext

      let! nextState, intent =
        updateState
          Async.singleton
          (createServices dbContext)
          creator.BotState
          event

      do! updateCreator dbContext { creator with BotState = nextState }

      do! handleIntent botConfig dbContext chatId name creatorId intent }
  |> Async.Ignore
