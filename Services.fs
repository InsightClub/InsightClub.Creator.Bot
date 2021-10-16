module InsightClub.Creator.Bot.Services

open Core


let get connection creatorId =
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
    let content, contentType =
      match content with
      | Text text -> text, "text"
      | Photo fileId -> fileId, "photo"
      | Audio fileId -> fileId, "audio"
      | Video fileId -> fileId, "video"
      | Voice fileId -> fileId, "voice"
      | Document fileId -> fileId, "document"
      | VideoNote fileId -> fileId, "video_note"

    do! Repo.addContent connection blockId content contentType

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
    checkAnyBlocks = checkAnyBlocks }
