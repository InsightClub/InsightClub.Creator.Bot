module InsightClub.Creator.Bot.Services

open Core


let get connection creatorId =
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

  { tryCreateCourse = tryCreateCourse
    tryUpdateTitle = tryUpdateTitle
    getCourseTitle = getCourseTitle
    getCourseDesc = getCourseDesc
    updateDesc = updateDesc }
