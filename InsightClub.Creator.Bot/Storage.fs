module InsightClub.Creator.Bot.Storage

open Amazon.S3
open Amazon.S3.Model
open System
open System.Net.Http


type Storage(client: AmazonS3Client, bucketName: String) =
    let mutable isDisposed = false

    member _.Client = client

    member _.BucketName = bucketName

    interface IDisposable with
        member this.Dispose() =
            if isDisposed then
                client.Dispose()
                isDisposed <- true

let saveFile botToken filePath (storage: Storage) fileId =
    task {
        let url = $"https://api.telegram.org/file/bot{botToken}/{filePath}"

        use http = new HttpClient()

        use! res = http.GetAsync(url)

        let req =
            PutObjectRequest(
                BucketName = storage.BucketName,
                Key = fileId,
                InputStream = res.Content.ReadAsStream()
            )

        let! _ = storage.Client.PutObjectAsync(req)

        return ()
    }
    |> Async.AwaitTask

let getFile (storage: Storage) fileId =
    task {
        let req =
            GetObjectRequest(BucketName = storage.BucketName, Key = fileId)

        let! res = storage.Client.GetObjectAsync(req)

        return res.ResponseStream
    }
    |> Async.AwaitTask

let deleteFile (storage: Storage) fileId =
    task {
        let req =
            DeleteObjectRequest(BucketName = storage.BucketName, Key = fileId)

        let! _ = storage.Client.DeleteObjectAsync(req)

        return ()
    }
    |> Async.AwaitTask
