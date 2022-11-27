module InsightClub.Creator.Bot.Repo

open Bot
open System
open Npgsql
open Npgsql.FSharp


module private Sql =
    let contentType t =
        let parameter =
            NpgsqlParameter("content_type", (t: string), DataTypeName = "content_type")

        Sql.parameter parameter

let private deserializeContent content =
    function
    | "text" -> Text content
    | "photo" -> Photo content
    | "audio" -> Audio content
    | "video" -> Video content
    | "voice" -> Voice content
    | "document" -> Document content
    | "video_note" -> VideoNote content
    | contentType -> failwith $"Unknown content type: {contentType}! FileId: {content}"

let private serializeContent =
    function
    | Text text -> text, "text"
    | Photo fileId -> fileId, "photo"
    | Audio fileId -> fileId, "audio"
    | Video fileId -> fileId, "video"
    | Voice fileId -> fileId, "voice"
    | Document fileId -> fileId, "document"
    | VideoNote fileId -> fileId, "video_note"

let tryCreateCourse connection creatorId courseTitle =
    async {
        try
            return!
                connection
                |> Sql.existingConnection
                |> Sql.query
                    "INSERT INTO courses(creator_id, course_title)
                    VALUES (@creator_id, @course_title)
                    RETURNING course_id"
                |> Sql.parameters [ "creator_id", Sql.int creatorId
                                    "course_title", Sql.string courseTitle ]
                |> Sql.executeRowAsync (fun read -> read.int "course_id")
                |> Async.AwaitTask
                |> Async.map Ok

        with
        | :? AggregateException as e when
            (e.InnerException :? PostgresException)
            && (e.InnerException :?> PostgresException).SqlState = PostgresErrorCodes.UniqueViolation
            ->
            return Error TitleError.NonUnique

        | :? AggregateException as e when
            (e.InnerException :? PostgresException)
            && (e.InnerException :?> PostgresException).SqlState = PostgresErrorCodes.CheckViolation
            ->
            return Error TitleError.TooLong
    }

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
    |> Sql.parameters [
        "telegram_id", Sql.int64 telegramId
        "initial_state", Sql.string initialState
    ]
    |> Sql.executeRowAsync (fun read -> read.int "creator_id", read.string "telegram_bot_state")
    |> Async.AwaitTask

let updateState connection creatorId state =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "UPDATE creators
        SET telegram_bot_state = @new_state
        WHERE creator_id = @creator_id"
    |> Sql.parameters [
        "new_state", Sql.string state
        "creator_id", Sql.int creatorId
    ]
    |> Sql.executeNonQueryAsync
    |> Async.AwaitTask
    |> Async.Ignore

let tryUpdateTitle connection courseId courseTitle =
    async {
        try
            return!
                connection
                |> Sql.existingConnection
                |> Sql.query
                    "UPDATE courses
                    SET course_title = @course_title
                    WHERE course_id = @course_id"
                |> Sql.parameters [
                    "course_title", Sql.string courseTitle
                    "course_id", Sql.int courseId
                ]
                |> Sql.executeNonQueryAsync
                |> Async.AwaitTask
                |> Async.map (Ok() |> always)
        with
        | :? AggregateException as e when
            (e.InnerException :? PostgresException)
            && (e.InnerException :?> PostgresException).SqlState = PostgresErrorCodes.UniqueViolation
            ->
            return Error TitleError.NonUnique

        | :? AggregateException as e when
            (e.InnerException :? PostgresException)
            && (e.InnerException :?> PostgresException).SqlState = PostgresErrorCodes.CheckViolation
            ->
            return Error TitleError.TooLong
    }

let getCourseTitle connection courseId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT course_title
        FROM courses
        WHERE course_id = @course_id"
    |> Sql.parameters [ "course_id", Sql.int courseId ]
    |> Sql.executeRowAsync (fun read -> read.string "course_title")
    |> Async.AwaitTask

let getCourseDesc connection courseId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT course_description
        FROM courses
        WHERE course_id = @course_id"
    |> Sql.parameters [ "course_id", Sql.int courseId ]
    |> Sql.executeRowAsync (fun read -> read.string "course_description")
    |> Async.AwaitTask

let tryUpdateDesc connection courseId courseDesc =
    async {
        try
            return!
                connection
                |> Sql.existingConnection
                |> Sql.query
                    "UPDATE courses
                    SET course_description = @course_description
                    WHERE course_id = @course_id"
                |> Sql.parameters [ "course_description", Sql.string courseDesc
                                    "course_id", Sql.int courseId ]
                |> Sql.executeNonQueryAsync
                |> Async.AwaitTask
                |> Async.map (always true)
        with
        | :? AggregateException as e when
            (e.InnerException :? PostgresException)
            && (e.InnerException :?> PostgresException).SqlState = PostgresErrorCodes.CheckViolation
            ->
            return false
    }

let checkAnyCourses connection creatorId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT EXISTS(
            SELECT 1
            FROM courses
            WHERE creator_id = @creator_id
        ) as any"
    |> Sql.parameters [ "creator_id", Sql.int creatorId ]
    |> Sql.executeRowAsync (fun read -> read.bool "any")
    |> Async.AwaitTask

let getCoursesCount connection creatorId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT COUNT(*) as count
        FROM courses
        WHERE creator_id = @creator_id"
    |> Sql.parameters [ "creator_id", Sql.int creatorId ]
    |> Sql.executeRowAsync (fun read -> read.int "count")
    |> Async.AwaitTask

let getCourses connection creatorId page count =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT course_id, course_title
        FROM courses
        WHERE creator_id = @creator_id
        ORDER BY course_id
        LIMIT @limit
        OFFSET @offset"
    |> Sql.parameters [
        "creator_id", Sql.int creatorId
        "limit", Sql.int count
        "offset", Sql.int (page * count)
    ]
    |> Sql.executeAsync (fun read -> read.int "course_id", read.string "course_title")
    |> Async.AwaitTask

let tryCreateBlock connection courseId blockIndex blockTitle =
    async {
        try
            use! transaction =
                (connection: NpgsqlConnection)
                    .BeginTransactionAsync()
                    .AsTask()
                |> Async.AwaitTask

            do!
                connection
                |> Sql.existingConnection
                |> Sql.query
                    $"
                    DO $$
                    DECLARE
                        c CURSOR FOR
                            SELECT block_id
                            FROM blocks
                            WHERE course_id = {courseId}
                            AND block_index >= {blockIndex}
                            ORDER BY block_index DESC
                            FOR UPDATE;
                    BEGIN
                        FOR row IN c LOOP
                            UPDATE blocks
                            SET block_index = block_index + 1
                            WHERE CURRENT OF c;
                        END LOOP;
                    END
                    $$
                    "
                |> Sql.executeNonQueryAsync
                |> Async.AwaitTask
                |> Async.Ignore

            let! blockId =
                connection
                |> Sql.existingConnection
                |> Sql.query
                    "INSERT INTO blocks(course_id, block_index, block_title)
                    VALUES (@course_id, @block_index, @block_title)
                    RETURNING block_id"
                |> Sql.parameters [
                    "course_id", Sql.int courseId
                    "block_index", Sql.int blockIndex
                    "block_title", Sql.string blockTitle
                ]
                |> Sql.executeRowAsync (fun read -> read.int "block_id")
                |> Async.AwaitTask

            do!
                transaction.CommitAsync()
                |> Async.AwaitTask

            return Some blockId
        with
        | :? AggregateException as e when
            (e.InnerException :? PostgresException)
            && (e.InnerException :?> PostgresException).SqlState = PostgresErrorCodes.UniqueViolation
            ->
            return None
    }

let addContent connection blockId content =
    let content, contentType =
        serializeContent content

    connection
    |> Sql.existingConnection
    |> Sql.query
        "INSERT INTO contents(block_id, content_index, content, content_type)
        VALUES (
            @block_id,
            (
                SELECT COALESCE(
                    (
                        SELECT MAX(content_index)
                        FROM contents
                        WHERE block_id = @block_id
                    ) + 1,
                    0
                )
            ),
            @content,
            @content_type
        )"
    |> Sql.parameters [
        "block_id", Sql.int blockId
        "content", Sql.string content
        "content_type", Sql.contentType contentType
    ]
    |> Sql.executeNonQueryAsync
    |> Async.AwaitTask
    |> Async.Ignore

let getBlockInfo connection blockId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT block_index, block_title
        FROM blocks
        WHERE block_id = @block_id"
    |> Sql.parameters [ "block_id", Sql.int blockId ]
    |> Sql.executeRowAsync (fun read -> read.int "block_index", read.string "block_title")
    |> Async.AwaitTask

let getBlocksCount connection courseId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT COUNT(*) as count
        FROM blocks
        WHERE course_id = @course_id"
    |> Sql.parameters [ "course_id", Sql.int courseId ]
    |> Sql.executeRowAsync (fun read -> read.int "count")
    |> Async.AwaitTask

let checkAnyBlocks connection courseId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT EXISTS(
          SELECT 1
          FROM blocks
          WHERE course_id = @course_id
        ) as any"
    |> Sql.parameters [ "course_id", Sql.int courseId ]
    |> Sql.executeRowAsync (fun read -> read.bool "any")
    |> Async.AwaitTask

let getBlocks connection courseId page count =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT block_id, block_title
        FROM blocks
        WHERE course_id = @course_id
        ORDER BY block_index
        LIMIT @limit
        OFFSET @offset"
    |> Sql.parameters [
        "course_id", Sql.int courseId
        "limit", Sql.int count
        "offset", Sql.int (page * count)
    ]
    |> Sql.executeAsync (fun read -> read.int "block_id", read.string "block_title")
    |> Async.AwaitTask

let getBlockContents connection blockId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT content, content_type
        FROM contents
        WHERE block_id = @block_id"
    |> Sql.parameters [ "block_id", Sql.int blockId ]
    |> Sql.executeAsync (
        fun read ->
            let content = read.string "content"
            let contentType = read.string "content_type"

            deserializeContent content contentType
    )
    |> Async.AwaitTask

let getBlockInfoByIndex connection courseId blockIndex =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "SELECT block_id, block_title
        FROM blocks
        WHERE course_id = @course_id
        AND block_index = @block_index"
    |> Sql.parameters [
        "course_id", Sql.int courseId
        "block_index", Sql.int blockIndex
    ]
    |> Sql.executeRowAsync (fun read -> read.int "block_id", read.string "block_title")
    |> Async.AwaitTask

let cleanBlock connection blockId =
    connection
    |> Sql.existingConnection
    |> Sql.query
        "DELETE FROM contents
        WHERE block_id = @block_id"
    |> Sql.parameters [ "block_id", Sql.int blockId ]
    |> Sql.executeNonQueryAsync
    |> Async.AwaitTask
