module InsightClub.Creator.Bot.Repo

open FsToolkit.ErrorHandling
open Npgsql
open Npgsql.FSharp


let tryCreateCourse connection creatorId courseTitle =
  try
    connection
    |> Sql.existingConnection
    |> Sql.query
      "INSERT INTO courses(creator_id, course_title)
      VALUES (@creator_id, @course_title)
      RETURNING course_id"
    |> Sql.parameters
      [ "creator_id", Sql.int creatorId
        "course_title", Sql.string courseTitle ]
    |> Sql.executeRowAsync (fun read -> read.int "course_id")
    |> Async.AwaitTask
    |> Async.map Some
  with
    :? PostgresException as px
      when px.ErrorCode = int PostgresErrorCodes.UniqueViolation ->
      Async.singleton None
