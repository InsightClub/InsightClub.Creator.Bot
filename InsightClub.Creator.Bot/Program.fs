module InsightClub.Creator.Bot.Program

open Amazon.Runtime
open Amazon.S3
open Config
open Funogram.Api
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Types
open Npgsql
open Npgsql.FSharp
open System
open System.Net


let startBot
    (config: Config)
    (listener: HttpListener)
    (connectToDb: unit -> NpgsqlConnection)
    (connectToStorage: unit -> Storage.Storage)
    =
    let apiPath = $"api/{config.BotToken}"

    let webhookUrl =
        config.WebhookAddress + apiPath

    let validate (req: HttpListenerRequest) = req.Url.LocalPath = $"/{apiPath}"

    let webhook = {
        Listener = listener
        ValidateRequest = validate
    }

    let botConfig = {
        Config.defaultConfig with
            Token = config.BotToken
            WebHook = Some webhook
    }

    let printError e =
        failwith $"Failed creating webhook on %s{webhookUrl}: %A{e}"

    let printStarted () =
        printfn $"Bot started! Listening to %s{config.BotEndPoint}"

    let setWebhook () =
        Req.SetWebhook.Make(webhookUrl)
        |> api botConfig
        |> Async.map (Result.mapError printError)
        |> Async.Ignore

    let startBot () =
        printStarted ()
        startBot botConfig (Handler.onUpdate connectToDb connectToStorage) None

    async {
        do! setWebhook ()
        do! startBot ()
    }
    |> Async.Ignore

[<EntryPoint>]
let main _ =
    let config = Config.load ()

    use listener = new HttpListener()
    listener.Prefixes.Add(config.BotEndPoint)

    let connectToDb () =
        config.DatabaseUrl
        |> Uri
        |> Sql.fromUriToConfig
        |> Sql.sslMode SslMode.Require
        |> Sql.trustServerCertificate true
        |> Sql.formatConnectionString
        |> Sql.connect
        |> Sql.createConnection

    // Test connection
    using (connectToDb ()) (fun c -> c.Open())

    let connectToStorage () =
        let conf =
            AmazonS3Config(
                ServiceURL = "https://s3.filebase.com:443",
                UseHttp = true,
                ForcePathStyle = true
            )

        let creds =
            BasicAWSCredentials(config.FilebaseAccessKey, config.FilebaseSecretKey)

        new Storage.Storage(new AmazonS3Client(creds, conf), config.FilebaseBucketName)

    // Run synchronously to block the tread
    // Don't use Async.StartImmediate or the
    // program will immediately shut after the launch
    startBot config listener connectToDb connectToStorage
    |> Async.Ignore
    |> Async.RunSynchronously

    0 // Return an integer exit code
