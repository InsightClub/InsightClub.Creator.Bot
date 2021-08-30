namespace InsightClub.Creator.Bot

open System.IO
open FSharp.Configuration


type internal Config = YamlConfig<"Default.Config.yaml">

module internal Config =
  let tryLoad filePath =
    let config = Config()

    if (File.Exists filePath) then
      config.Load(filePath)
      Some config
    else
      None
