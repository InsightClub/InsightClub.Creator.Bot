module InsightClub.Creator.Bot.Services

open Bot
open Funogram
open Funogram.Telegram


let get connection config storage creatorId =
  let tryCreateCourse courseTitle callback = async {
    let! courseIdOption =
      Repo.tryCreateCourse connection creatorId courseTitle

    return! callback courseIdOption }

  let tryUpdateTitle courseId courseTitle callback = async {
    let! isUpdated =
      Repo.tryUpdateTitle connection courseId courseTitle

    return! callback isUpdated }

  let tryUpdateDesc courseId courseDesc callback = async {
    let! isUpdated =
      Repo.tryUpdateDesc connection courseId courseDesc

    return! callback isUpdated }

  let checkAnyCourses callback = async {
    let! isAny =
      Repo.checkAnyCourses connection creatorId

    return! callback isAny }

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
          storage
          file.FileId
        |> Async.Start

      | Error e ->
        failwith <| $"Error getting file: %A{e}"

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
    let! isAny =
      Repo.checkAnyBlocks connection courseId

    return! callback isAny }

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

    // In future versions use batch delete
    for content in contents do
      if content.IsFile then
        do! Storage.deleteFile storage content.Content

    let! count =
      Repo.cleanBlock connection blockId

    return! callback (count > 0) }

  { tryCreateCourse = tryCreateCourse
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
