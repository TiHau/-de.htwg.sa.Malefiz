package de.htwg.sa.malefiz.model.db

import org.mongodb.scala._
import play.api.libs.json.{JsBoolean, JsValue, Json}

object GameConfigDao {
  val mongoClient: MongoClient = MongoClient()
  val database: MongoDatabase = mongoClient.getDatabase("malefiz")
  val collection: MongoCollection[Document] = database.getCollection("saves")

  def getLatestSave(): JsValue = {
    return JsBoolean(true)
  }

  def insert(name: String, config: JsValue): Unit = {
    collection.insertOne(Document(Json.obj("name" -> name, "config" -> config).toString()))
  }
}
