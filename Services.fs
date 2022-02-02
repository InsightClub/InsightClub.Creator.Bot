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

  let tryUpdateDesc courseId courseDesc callback = async {
    let! ok =
      Repo.tryUpdateDesc connection courseId courseDesc

    return! callback ok }

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

  let addContent blockId content callback = async {
    do!
      Repo.addContent connection blockId content

    if content.IsFile then
      let! file =
        Api.getFile content.Content
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

    return! callback contents }

  let getBlockInfoByIndex courseId blockId callback = async {
    let! info =
      Repo.getBlockInfoByIndex connection courseId blockId

    return! callback info }

  let cleanBlock blockId callback = async {
    let! contents =
      Repo.getBlockContents connection blockId

    contents
    |> List.iter
      ( fun content ->
          if content.IsFile then
            Storage.deleteFile storagePath content.Content )

    let! count =
      Repo.cleanBlock connection blockId

    return! callback (count > 0) }

  { callback = callback
    tryCreateCourse = tryCreateCourse
    tryUpdateTitle = tryUpdateTitle
    tryUpdateDesc = tryUpdateDesc
    checkAnyCourses = checkAnyCourses
    getCoursesCount = getCoursesCount
    tryCreateBlock = tryCreateBlock
    addContent = addContent
    getBlockInfo = getBlockInfo
    getBlocksCount = getBlocksCount
    checkAnyBlocks = checkAnyBlocks
    getBlockContents = getBlockContents
    getBlockInfoByIndex = getBlockInfoByIndex
    cleanBlock = cleanBlock }
