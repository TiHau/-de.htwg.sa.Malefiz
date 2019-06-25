package de.htwg.sa.malefiz.model.db

import de.htwg.se.malefiz.model.db.DaoInterface
import play.api.libs.json.{JsValue, Json}
import slick.jdbc.H2Profile.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class GameConfigDaoSlick() extends DaoInterface{
  private lazy val db = Database.forConfig("h2mem1")
  private lazy val gameConfigs = TableQuery[GameConfigTable]

  override def getLatestSave: JsValue = {
    val f = db.run(gameConfigs.sortBy(_.id.desc).take(1).result.head)
    val res: GameConfig = Await.result(f, Duration.Inf)
    Json.parse(res.config)
  }

  override def insert(name:String, saveJson:JsValue): Unit = {
    val gameConfig = GameConfig(name, saveJson.toString())
    Await.result(db.run(gameConfigs.schema.createIfNotExists), Duration.Inf)
    Await.result(db.run(gameConfigs += gameConfig),Duration.Inf)
  }
}