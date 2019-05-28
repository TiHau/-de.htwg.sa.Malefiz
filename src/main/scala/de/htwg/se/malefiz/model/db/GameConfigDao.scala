package de.htwg.sa.malefiz.model.db

import slick.jdbc.H2Profile.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object GameConfigDao {
  private lazy val db = Database.forConfig("h2mem1")
  private lazy val gameConfigs = TableQuery[GameConfigTable]

  def getLatestSave(): Future[GameConfig] = {
    db.run(gameConfigs.sortBy(_.id.desc).take(1).result.head)
  }

  def insert(gameConfig: GameConfig): Future[Int] = {
    Await.result(db.run(gameConfigs.schema.createIfNotExists), Duration.Inf)
    db.run(gameConfigs += gameConfig)
  }
}
