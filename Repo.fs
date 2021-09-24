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

let getState initialState connection telegramId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "WITH i AS(
      INSERT INTO creators (telegram_id, telegram_bot_state)
      VALUES (@telegram_id, @initial_state)
      ON CONFLICT(telegram_id)
      DO NOTHING
      RETURNING creator_id, telegram_bot_state
    )
    SELECT * FROM i
    UNION
    SELECT creator_id, telegram_bot_state
    FROM creators
    WHERE telegram_id = @telegram_id"
  |> Sql.parameters
    [ "telegram_id", Sql.int64 telegramId
      "initial_state", Sql.string initialState ]
  |> Sql.executeRowAsync
    ( fun read ->
        read.int "creator_id",
        read.string "telegram_bot_state" )
  |> Async.AwaitTask

let updateState connection creatorId newState =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "UPDATE creators
    SET telegram_bot_state = @new_state
    WHERE creator_id = @creator_id"
  |> Sql.parameters
    [ "new_state", Sql.string newState
      "creator_id", Sql.int creatorId ]
  |> Sql.executeNonQueryAsync
  |> Async.AwaitTask
  |> Async.Ignore
