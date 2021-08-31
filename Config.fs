module InsightClub.Creator.Bot.Config

open System.IO
open FSharp.Configuration


type Config = YamlConfig<"Default.Config.yaml">

module Config =
  let tryLoad filePath =
    let config = Config()

    if (File.Exists filePath) then
      config.Load(filePath)
      Some config
    else
      None

  let getConnectionString (config: Config) =
    $"Host={config.Db.Host};
      Database={config.Db.Database};
      Username={config.Db.Username};
      Password={config.Db.Password}"
