module InsightClub.Creator.Bot.Storage

open System.IO
open System.Net


let saveFile botToken filePath storagePath fileId =
  let wc = new WebClient()
  wc.DownloadFileTaskAsync(
    $"https://api.telegram.org/file/bot{botToken}/{filePath}",
    Path.Combine([| storagePath; fileId |])
  )
  |> Async.AwaitTask
