package de.htwg.se.malefiz.controller

import de.htwg.se.malefiz.model.gameboard._
import de.htwg.se.malefiz.util.Observable
import play.api.libs.json.JsObject

import scala.swing.Publisher

trait ControllerInterface extends Observable with Publisher {

  /**
   * läd den Gespeicherten Spielstand
   */
  def loadSavedGame(): Unit

  /**
   * Speichert den aktuellen Spielstand ab
   */
  def saveGame(): Unit

  /**
   * Erstellt ein neues Spiel
   * @param countPlayer Spieleranzahl
   */
  def newGame(countPlayer: Int): Unit

  /**
   * Setzt die Spieleranzahl
   * @param playerCount Spieleranzahl
   */
  def setPlayerCount(playerCount: Int): Unit


  /**
   * Bekommt Koordinaten eines Felds und führt in Abhängigkeit des aktuellen Spielstatuses
   * die jeweilige Operationen aus
   * @param x X-Koordinate
   * @param y Y-Koordinate
   */
  def takeInput(x: Int, y: Int): Unit



  def toJson: JsObject
}
