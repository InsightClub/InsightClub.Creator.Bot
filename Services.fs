module InsightClub.Creator.Bot.Services

open Core
open Funogram
open Funogram.Telegram


let get connection config storagePath creatorId =
  let callback state effect =
    Async.singleton (state, effect)

  let tryCreateCourse courseTitle callback = async {
    let! courseIdOption =
      Repo.tryCreateCourse connection creatorId courseTitle

    return! callback courseIdOption }

  let tryUpdateTitle courseId courseTitle callback = async {
    let! wasUpdated =
      Repo.tryUpdateTitle connection courseId courseTitle

    return! callback wasUpdated }

  let getCourseTitle courseId callback = async {
    let! courseTitle =
      Repo.getCourseTitle connection courseId

    return! callback courseTitle }

  let getCourseDesc courseId callback = async {
    let! desc =
      Repo.getCourseDesc connection courseId

    return! callback desc }

  let updateDesc courseId courseDesc callback = async {
    do! Repo.updateDesc connection courseId courseDesc

    return! callback () }

  let checkAnyCourses callback = async {
    let! any =
      Repo.checkAnyCourses connection creatorId

    return! callback any }

  let getCoursesCount callback = async {
    let! count =
      Repo.getCoursesCount connection creatorId

    return! callback count }

  let tryCreateBlock courseId index blockTitle callback = async {
    let! blockId =
      Repo.tryCreateBlock connection courseId index blockTitle

    return! callback blockId }

  let getLastBlockIndex courseId callback = async {
    let! lastIndex =
      Repo.getLastBlockIndex connection courseId

    return! callback lastIndex }

  let addContent blockId content callback = async {
    let innerContent, contentType =
      match content with
      | Text text        -> text, "text"
      | Photo fileId     -> fileId, "photo"
      | Audio fileId     -> fileId, "audio"
      | Video fileId     -> fileId, "video"
      | Voice fileId     -> fileId, "voice"
      | Document fileId  -> fileId, "document"
      | VideoNote fileId -> fileId, "video_note"

    do! Repo.addContent connection blockId innerContent contentType

    if content.IsFile then
      let! file =
        Api.getFile innerContent
        |> Api.api config

      match file with
      | Ok file ->
        Storage.saveFile
          config.Token
          file.FilePath.Value
          storagePath
          file.FileId
        |> Async.Start

      | Error e ->
        failwith <| sprintf "Error getting file: %A" e

    return! callback () }

  let getBlockInfo blockId callback = async {
    let! info =
      Repo.getBlockInfo connection blockId

    return! callback info }

  let getBlocksCount courseId callback = async {
    let! count =
      Repo.getBlocksCount connection courseId

    return! callback count }

  let checkAnyBlocks courseId callback = async {
    let! any =
      Repo.checkAnyBlocks connection courseId

    return! callback any }

  let getBlockContents blockId callback = async {
    let! contents =
      Repo.getBlockContents connection blockId

    let contents =
      contents
      |> List.map
        ( function
          | text, "text"         -> Text text
          | fileId, "photo"      -> Photo fileId
          | fileId, "audio"      -> Audio fileId
          | fileId, "video"      -> Video fileId
          | fileId, "voice"      -> Voice fileId
          | fileId, "document"   -> Document fileId
          | fileId, "video_note" -> VideoNote fileId
          | fileId, contentType  ->
            failwith $"Unknown content type: {contentType}! FileId: {fileId}" )

    return! callback contents }

  { callback = callback
    tryCreateCourse = tryCreateCourse
    tryUpdateTitle = tryUpdateTitle
    getCourseTitle = getCourseTitle
    getCourseDesc = getCourseDesc
    updateDesc = updateDesc
    checkAnyCourses = checkAnyCourses
    getCoursesCount = getCoursesCount
    tryCreateBlock = tryCreateBlock
    getLastBlockIndex = getLastBlockIndex
    addContent = addContent
    getBlockInfo = getBlockInfo
    getBlocksCount = getBlocksCount
    checkAnyBlocks = checkAnyBlocks
    getBlockContents = getBlockContents }
