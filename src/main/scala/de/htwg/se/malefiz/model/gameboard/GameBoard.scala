package de.htwg.se.malefiz.model.gameboard

import com.google.inject.Inject
import com.google.inject.name.Named
import play.api.libs.json._

import scala.collection.{immutable, mutable}

case class GameBoard @Inject() (@Named("DefaultSize") playerCount: Int, board: Map[(Int, Int), Field] =
immutable.HashMap.empty[(Int, Int), Field]) extends GameBoardInterface {
  override def createBoard: GameBoard = {
    var tmp = defineField(8, 0).defineField(8, 4)
      .defineField(0, 2).defineField(16, 2)
      .defineField(6, 6).defineField(10, 6)
      .defineField(12, 8).defineField(4, 8)
      .defineField(2, 10).defineField(6, 10)
      .defineField(10, 10).defineField(14, 10)
    (0 to 16).foreach(i => tmp = tmp.defineField(i, 1)
      .defineField(i, 3).defineField(i, 11).defineField(i, 13))
    (6 to 10).foreach(i => tmp = tmp.defineField(i, 5))
    (4 to 12).foreach(i => tmp = tmp.defineField(i, 7))
    (2 to 14).foreach(i => tmp = tmp.defineField(i, 9))
    (0 to 16).filter(i => i % 4 == 0).foreach(i => tmp = tmp.defineField(i, 12))
    (1 to 15).filter(i => i % 4 != 0).foreach(i => tmp = tmp.defineField(i, 14))
    (0 to 16).filter(i => i % 2 != 0).foreach(i => tmp = tmp.defineField(i, 15))
    (1 to 5).filter(y => y != 2).foreach(y => tmp = tmp.defineBlockStone(8, y))
    tmp = tmp.defineBlockStone(6, 7).defineBlockStone(10, 7)
    (0 to 16).filter(i => i % 4 == 0).foreach(x => tmp = tmp.defineBlockStone(x, 11))
    tmp = tmp.definePlayerStones(1, player1).definePlayerStones(13, player4)
    if (playerCount >= 3) {
      tmp = tmp.definePlayerStones(5, player2)
      if (playerCount == 4) tmp = tmp.definePlayerStones(9, player3)
    }
    tmp
  }
  def defineField(x: Int, y: Int): GameBoard = copy(board = board + ((x, y) -> Field(x, y, None)))
  def defineBlockStone(x: Int, y: Int): GameBoard = copy(board = board + ((x, y) -> Field(x, y, Some(BlockStone()))))
  def definePlayerStones(xFirst: Int, player: Player): GameBoard = {
    copy(board = board + (
      (xFirst, 14) -> Field(xFirst, 14, Some(PlayerStone(xFirst, 14, xFirst, 14, player.color))),
      (xFirst, 15) -> Field(xFirst, 15, Some(PlayerStone(xFirst, 15, xFirst, 15, player.color))),
      (xFirst + 1, 14) -> Field(xFirst + 1, 14, Some(PlayerStone(xFirst + 1, 14, xFirst + 1, 14, player.color))),
      (xFirst + 2, 14) -> Field(xFirst + 2, 14, Some(PlayerStone(xFirst + 2, 14, xFirst + 2, 14, player.color))),
      (xFirst + 2, 15) -> Field(xFirst + 2, 15, Some(PlayerStone(xFirst + 2, 15, xFirst + 2, 15, player.color)))))
  }

  override def toString: String = {
    val jsb = new mutable.StringBuilder()
    (0 to 15).foreach(y => {
      if (y < 10) jsb.append(y + "  ") else jsb.append(y + " ")
      (0 to 16).foreach(i => {
        if (!board.contains((i, y))) jsb.append("   ")
        else {
          val s: Field = board((i, y))
          s.stone match {
            case Some(stone: PlayerStone) =>
              if (s.available) {
                stone.playerColor match {
                  case 1 => jsb.append("|G|")
                  case 2 => jsb.append("|H|")
                  case 3 => jsb.append("|J|")
                  case 4 => jsb.append("|K|")
                  case _ => jsb.append("|P|")
                }
              } else jsb.append("|" + stone.playerColor + "|")
            case Some(_: BlockStone) =>
              if (s.available) jsb.append("|B|") else jsb.append("|-|")
            case None =>
              if (s.available) jsb.append("|x|") else jsb.append("|o|")
            case _ =>
          }
        }
      })
      jsb.append("\n")
    })
    jsb.append("    ")
    (0 to 9).addString(jsb, "  ")
    jsb.append(" ")
    (10 to 16).addString(jsb, " ")
    jsb.toString()
  }

  override def toJson: JsObject = {
    Json.obj(
      "rows" -> Json.toJson(
        for {
          y <- 0 to 15
        } yield rowToJson(y)))
  }

  private def rowToJson(y: Int): JsObject = {
    Json.obj(
      "rowNr" -> JsNumber(y),
      "fields" -> Json.toJson(
        for {
          x <- 0 to 16
        } yield fieldToJson(x, y)))
  }
  private def fieldToJson(x: Int, y: Int): JsObject = {
    if (board.contains((x, y))) {
      val field = board((x, y))
      field.stone match {
        case Some(stone: PlayerStone) =>
          Json.obj(
            "isFreeSpace" -> JsBoolean(false),
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "sort" -> JsString(stone.playerColor.toString),
            "avariable" -> JsBoolean(field.available))
        case Some(_: BlockStone) =>
          Json.obj(
            "isFreeSpace" -> JsBoolean(false),
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "sort" -> JsString("b"),
            "avariable" -> JsBoolean(field.available))

        case None =>
          Json.obj(
            "isFreeSpace" -> JsBoolean(false),
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "sort" -> JsString("f"),
            "avariable" -> JsBoolean(field.available))
      }
    } else {
      Json.obj(
        "isFreeSpace" -> JsBoolean(true),
        "x" -> JsNumber(x),
        "y" -> JsNumber(y),
        "sort" -> JsString("f"),
        "avariable" -> JsBoolean(false))
    }
  }

  def moveStone(current: Field, dest: Field): (Boolean, Option[Stone], GameBoard) =
    if (validField(dest.x, dest.y) && board((dest.x, dest.y)).available) {
      val save = dest.stone
      (true, save, copy(board = board
        - ((dest.x, dest.y))
        + ((dest.x, dest.y) -> board((dest.x, dest.y)).copy(stone = Some(current.stone.get.asInstanceOf[PlayerStone].copy(x = dest.x, y = dest.y))))
        - ((current.x, current.y))
        + ((current.x, current.y) -> board((current.x, current.y)).copy(stone = None))).unmarkPossibleMoves())
    } else {
      (false, None, copy())
    }

  def forceMoveStone(current: Field, dest: Field): GameBoard =
    if (board.contains((dest.x, dest.y))) {
      val ps: PlayerStone = current.stone.get.asInstanceOf[PlayerStone]
      copy(board = board
        - ((dest.x, dest.y))
        + ((dest.x, dest.y) -> board((dest.x, dest.y)).copy(stone = Some(ps.copy(x = dest.x, y = dest.y))))
        - ((current.x, current.y))
        + ((current.x, current.y) -> board((current.x, current.y)).copy(stone = None)))
    } else {
      copy()
    }

  def resetPlayerStone(stone: PlayerStone): GameBoard =
    copy(board = board - ((stone.startX, stone.startY))
      + ((stone.startX, stone.startY) -> board((stone.startX, stone.startY)).copy(stone = Some(stone.copy(x = stone.startX, y = stone.startY)))))

  private def checkDestForBlockStone(x: Int, y: Int): Boolean = y < 12 && board.contains((x, y)) && board((x, y)).stone.isEmpty && (x, y) != (8, 0)

  def setBlockStoneOnField(field: Field): (Boolean, GameBoard) = {
    if (checkDestForBlockStone(field.x, field.y))
      (true,copy(board = board - ((field.x, field.y)) + ((field.x, field.y) -> board((field.x, field.y)).copy(stone = Some(BlockStone())))))
    else
      (false,copy())
  }

  def removeStoneOnField(field: Field): GameBoard = copy(board = board - ((field.x, field.y))
    + ((field.x, field.y) -> board((field.x, field.y)).copy(stone = None)))

  def markPossibleMoves(stone: PlayerStone, player: Player, diced: Int): GameBoard = {
    var tmp = copy().unmarkPossibleMoves()
    if (stone.isOnStart) {
      tmp = tmp.markPossibleMovesR(player.start._1, player.start._2, diced, ' ', player.color)
    } else {
      tmp = tmp.markPossibleMovesR(stone.x, stone.y, diced, ' ', player.color)
    }
    tmp
  }

  private def markPossibleMovesR(x: Int, y: Int, depth: Int, cameFrom: Char, playerColor: Int): GameBoard = {
    if (depth == 0) {
      //Dont hit your own kind
      if (board((x, y)).stone.isDefined && board((x, y)).stone.get.isInstanceOf[PlayerStone]
        && board((x, y)).stone.get.asInstanceOf[PlayerStone].playerColor == playerColor) {
        return copy()
      }
      copy(board = board - ((x, y)) + ((x, y) -> board((x, y)).copy(available = true)))
    } else {
      // If there is a blocking stone on the way dont go on
      if (board((x, y)).stone.isDefined && board((x, y)).stone.get.isInstanceOf[BlockStone]) {
        return copy()
      }
      var tmp = copy()
      // up
      if (validField(x, y - 1) && cameFrom != 'u') {
        tmp = tmp.markPossibleMovesR(x, y - 1, depth - 1, 'd', playerColor)
      }
      // down
      if (validField(x, y + 1) && cameFrom != 'd') {
        tmp = tmp.markPossibleMovesR(x, y + 1, depth - 1, 'u', playerColor)
      }
      // left
      if (validField(x - 1, y) && cameFrom != 'r') {
        tmp = tmp.markPossibleMovesR(x - 1, y, depth - 1, 'l', playerColor)
      }
      // right
      if (validField(x + 1, y) && cameFrom != 'l') {
        tmp = tmp.markPossibleMovesR(x + 1, y, depth - 1, 'r', playerColor)
      }
      tmp
    }
  }

  def unmarkPossibleMoves(): GameBoard = {
    var tmp: GameBoard = copy()
    val tmpB = tmp.board.seq
    tmpB.foreach(f => tmp = tmp.unmarkField(f._1._1, f._1._2))
    tmp
  }
  private def unmarkField(x: Int, y: Int):GameBoard = copy(board = board - ((x, y)) + ((x, y) -> board((x, y)).copy(available = false)))
  private def validField(x: Int, y: Int): Boolean = y <= 13 && board.contains((x, y))

  def checkDestForPlayerStone(x: Int, y: Int): Boolean = validField(x, y) && board((x, y)).available

  //wenn ein Stein im Zielfeld steht muss es ein Spielerstein sein => Sieg
  def checkWin: Boolean = board((8, 0)).stone.isDefined

  def setField(target: (Int, Int), whatToSet: Field): GameBoard = copy(board = board - target + (target -> whatToSet))
}