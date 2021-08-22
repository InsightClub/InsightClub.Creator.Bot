module InsightClub.Creator.Bot.Db

open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Builders
open MongoDB.FSharp


type Creator =
  { Id : BsonObjectId
    TelegramId: int64 }

let client = MongoClient(Config.Db.ConnectionString)
let db = client.GetDatabase(Config.Db.DbName)
let creatorsCollection = db.GetCollection<Creator>("creators")

let generateNewId () =
  BsonObjectId(ObjectId.GenerateNewId())
