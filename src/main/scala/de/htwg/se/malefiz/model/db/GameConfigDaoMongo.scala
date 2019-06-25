package de.htwg.se.malefiz.model.db

import org.mongodb.scala._
import play.api.libs.json.{JsBoolean, JsValue, Json}

case class GameConfigDaoMongo() extends DaoInterface {
  val mongoClient: MongoClient = MongoClient()
  val database: MongoDatabase = mongoClient.getDatabase("malefiz")
  val collection: MongoCollection[Document] = database.getCollection("saves")

  override def getLatestSave: JsValue = {
    var waitOnRes = true
    var res: JsValue = JsBoolean(true)
    val observable: Observable[Document] = collection.find().first()

    observable.subscribe(new Observer[Document] {
      override def onNext(result: Document): Unit = {
        res = Json.parse(result("config").toString)
      }

      override def onError(e: Throwable): Unit = println("Failed")

      override def onComplete(): Unit = {
        waitOnRes = false
        println("Completed")
      }
    })

    while(waitOnRes)
      Thread.sleep(10)

    res
  }

  override def insert(name: String, config: JsValue): Unit = {
    val observable: Observable[Completed] = collection.insertOne(Document(Json.obj("name" -> name, "config" -> config).toString()))

    observable.subscribe(new Observer[Completed] {

      override def onNext(result: Completed): Unit = println("Inserted")

      override def onError(e: Throwable): Unit = println("Failed")

      override def onComplete(): Unit = println("Completed")
    })
  }
}
