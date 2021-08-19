namespace Insight.Creator.Bot

open System.IO
open FSharp.Configuration

type Config = YamlConfig<"Default.Config.yaml">

[<AutoOpen>]
module Config =
  let private localConfigFilePath = "Config.yaml"

  let Config =
    let c = Config()

    if File.Exists localConfigFilePath then
      c.Load(localConfigFilePath)

    c
