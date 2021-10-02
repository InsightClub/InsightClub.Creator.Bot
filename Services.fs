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

  { tryCreateCourse = tryCreateCourse
    tryUpdateTitle = tryUpdateTitle }
