module InsightClub.Creator.Bot.Storage

open System.IO
open System.Net.Http


let private makePath storagePath fileId =
  Path.Combine([| storagePath; fileId |])

let saveFile botToken filePath storagePath fileId =
  let url =
    $"https://api.telegram.org/file/bot{botToken}/{filePath}"

  let http =
    new HttpClient()

  task {
    let! res =
      http.GetAsync(url)

    use fs =
      new FileStream(
        makePath storagePath fileId,
        FileMode.CreateNew
      )

    do!
      res.Content.CopyToAsync(fs)
  }
  |> Async.AwaitTask

let getFile storagePath fileId =
  makePath storagePath fileId
  |> File.OpenRead
  :> Stream

let deleteFile storagePath fileId =
  makePath storagePath fileId
  |> File.Delete
