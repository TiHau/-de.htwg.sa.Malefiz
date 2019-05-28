package de.htwg.sa.malefiz.model.db

import slick.jdbc.H2Profile.api._

case class GameConfig(name: String, config: String, id: Option[Int] = None)

class GameConfigTable(tag: Tag) extends Table[GameConfig](tag, "GAMECONFIGS") {
  // Auto Increment the id primary key column
  def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

  def name = column[String]("NAME")

  def config = column[String]("CONFIG")

  def * = (name, config, id.?) <> (GameConfig.tupled, GameConfig.unapply)
}
