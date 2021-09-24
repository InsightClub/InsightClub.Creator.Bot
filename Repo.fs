module InsightClub.Creator.Bot.Repo

open Npgsql.FSharp


let checkCourseTitleReserved connection creatorId courseTitle =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT EXISTS(
      SELECT 1
      FROM courses
      WHERE creator_id = @creator_id
      AND course_title = @course_title
    ) AS e"
  |> Sql.parameters
    [ "creator_id", Sql.int creatorId
      "course_title", Sql.string courseTitle ]
  |> Sql.executeRowAsync (fun read -> read.bool "e")
  |> Async.AwaitTask

let createCourse connection creatorId courseTitle =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "INSERT INTO courses(creator_id, course_title)
    VALUES (@creator_id, @course_title)"
  |> Sql.parameters
    [ "creator_id", Sql.int creatorId
      "course_title", Sql.string courseTitle ]
  |> Sql.executeNonQueryAsync
  |> Async.AwaitTask
