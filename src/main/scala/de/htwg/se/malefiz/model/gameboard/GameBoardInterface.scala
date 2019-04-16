package de.htwg.se.malefiz.model.gameboard

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.swing.Publisher

trait GameBoardInterface extends Publisher {
  private val one = 1
  private val two = 2
  private val three = 3
  private val four = 4

  /**
   * Spieler 1
   */
  val player1 = Player(one, (2, 14))
  /**
   * Spieler 2
   */
  val player2 = Player(two, (6, 14))
  /**
   * Spieler 3
   */
  val player3 = Player(three, (10, 14))
  /**
   * Spieler 4
   */
  val player4 = Player(four, (14, 14))

  /**
   * Das Spielfeld
   */
  val board: mutable.Map[(Int, Int), Field] = mutable.HashMap.empty[(Int, Int), Field]

  /**
   * Initialisiert Felder neu
   * @return neues GameBoard
   */
  def createBoard: GameBoardInterface

  /**
   * Liefert die Anzahl von Spielern
   * @return Anzahl
   */
  def playerCount: Int

  /**
   * Überprüft ob an den übergebenen Koordinaten ein Blockstein gesetzt werden darf
   * @param x X-Koordinate des zu überprüfenden Feldes
   * @param y y-Koordinate des zu überprüfenden Feldes
   * @return Liefert true, wenn ein Blockstein auf dieses Feld gesetzt werden darf
   */
  def checkDestForBlockStone(x: Int, y: Int): Boolean

  /**
   * Setzt einen Blockstein auf ein übergebenes Feld. Das Feld sollte vorher mit
   * <code>checkDestForBlockStone(x: Int, y: Int): Boolean</code> überprüft werden.
   * @param field Ein Freies Feld
   */
  def setBlockStoneOnField(field: Field): Unit

  /**
   * Entfernt den Stein des Felds und setzt ihn auf einen FreeStone
   * @param field Ein Feld
   */
  def removeStoneOnField(field: Field): Unit

  /**
   * Setzt den übergebenen Spielerstein wieder auf sein Startfeld zurück
   * @param stone Ein Spielerstein
   */
  def resetPlayerStone(stone: PlayerStone): Unit

  /**
   * Überprüft ob an den übergebenen Koordinaten ein Spielerstein gesetzt werden darf
   * @param x X-Koordinate des zu überprüfenden Feldes
   * @param y y-Koordinate des zu überprüfenden Feldes
   * @return Liefert true, wenn ein Blockstein auf dieses Feld gesetzt werden darf
   */
  def checkDestForPlayerStone(x: Int, y: Int): Boolean

  /**
   * Versetzt den Stein von f1 nach f2. Überprüft ob das Zielfeld im Spielbereich liegt und ob avaribale= true ist
   * Der Stein auf f1 wird auf FreeStone gesetzt.
   * @param current Aktuelles Feld
   * @param dest Zielfeld
   * @return Den geschlagenen Stein oder None
   */
  def moveStone(current: Field, dest: Field): Option[Stone]

  /**
   * Versetzt einene Stein.Ohne das Zielfeld zu überprüfen.
   * Der Stein auf f1 wird auf FreeStone gesetzt.
   * @param current Aktuelles Feld
   * @param dest Zielfeld
   */
  def forceMoveStone(current: Field, dest: Field): Unit

  /**
   * Setzt avariable auf anwählbare Felder einens Steins in abhängigkeit von der gewürfelten Zahl
   * @param stone Der Stein
   * @param player Der besitzer des Steins(aktiver Spieler)
   * @param diced Die Distanz
   */
  def markPossibleMoves(stone: PlayerStone, player: Player, diced: Int): Unit

  /**
   * Setzt avariable von allen Feldern auf false
   */
  def unmarkPossibleMoves(): Unit

  /**
   * Überpüft ob ein Stein auf dem Zielfeld steht
   * @return true, wenn ein Spielerstein auf dem Zielfed steht
   */
  def checkWin: Boolean

}
