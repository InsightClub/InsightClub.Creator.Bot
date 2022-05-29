module InsightClub.Creator.Bot.Storage

open System
open System.IO
open System.Net.Http
open Dropbox.Api


let private makePath fileName =
  Path.Combine([| "/"; fileName |])

let saveFile botToken filePath dropboxAccessToken fileId =
  task {
    let url =
      $"https://api.telegram.org/file/bot{botToken}/{filePath}"

    use http =
      new HttpClient()

    let! res =
      http.GetAsync(url)

    use dropbox = new DropboxClient(dropboxAccessToken)

    let! _ =
      dropbox.Files.UploadAsync(
        makePath fileId,
        body = res.Content.ReadAsStream()
      )

    return ()
  }
  |> Async.AwaitTask

let getFile dropboxAccessToken fileId =
  task {
    use dropbox = new DropboxClient(dropboxAccessToken)

    let! file = dropbox.Files.DownloadAsync(makePath fileId)

    return! file.GetContentAsStreamAsync()
  }
  |> Async.AwaitTask

let deleteFile dropboxAccessToken fileId =
  task {
    use dropbox = new DropboxClient(dropboxAccessToken)

    let! _ = dropbox.Files.DeleteV2Async(makePath fileId)

    return ()
  }
  |> Async.AwaitTask
