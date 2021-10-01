module InsightClub.Creator.Bot.Repo

open Npgsql
open Npgsql.FSharp
open System

let tryCreateCourse connection creatorId courseTitle =
  async
    { try
        return!
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
      | :? AggregateException as e when
        (e.InnerException :? PostgresException) &&
        (e.InnerException :?> PostgresException).SqlState
          = PostgresErrorCodes.UniqueViolation ->
        return None }

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

let tryUpdateTitle connection courseId newTitle =
  async
    { try
        return!
          connection
          |> Sql.existingConnection
          |> Sql.query
            "UPDATE courses
            SET course_title = @course_title
            WHERE course_id = @course_id"
          |> Sql.parameters
            [ "course_title", Sql.string newTitle
              "course_id", Sql.int courseId ]
          |> Sql.executeNonQueryAsync
          |> Async.AwaitTask
          |> Async.map(fun n -> n > 0)
      with
      | :? AggregateException as e when
        (e.InnerException :? PostgresException) &&
        (e.InnerException :?> PostgresException).SqlState
          = PostgresErrorCodes.UniqueViolation ->
        return false }
