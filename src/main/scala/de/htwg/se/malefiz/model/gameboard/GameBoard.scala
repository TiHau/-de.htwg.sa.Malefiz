package de.htwg.se.malefiz.model.gameboard

import com.google.inject.Inject
import com.google.inject.name.Named
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.concurrent.Future
import scala.swing.Publisher

case class GameBoard @Inject() (@Named("DefaultSize") var playerCount: Int) extends GameBoardInterface with Publisher {
  override def createBoard: Future[GameBoard] = Future{
    defineField(8, 0)
    defineField(8, 4)
    (0 to 16).foreach(i => defineField(i, 1))
    (0 to 16).foreach(i => defineField(i, 3))
    defineField(0, 2)
    defineField(16, 2)
    (6 to 10).foreach(i => defineField(i, 5))
    defineField(6, 6)
    defineField(10, 6)
    (4 to 12).foreach(i => defineField(i, 7))
    defineField(12, 8)
    defineField(4, 8)
    (2 to 14).foreach(i => defineField(i, 9))
    defineField(2, 10)
    defineField(6, 10)
    defineField(10, 10)
    defineField(14, 10)
    (0 to 16).foreach(i => defineField(i, 11))
    (0 to 16).foreach(i => defineField(i, 13))
    (0 to 16).filter(i => i % 4 == 0).foreach(i => defineField(i, 12))
    (1 to 15).filter(i => i % 4 != 0).foreach(i => defineField(i, 14))
    (0 to 16).filter(i => i % 2 != 0).foreach(i => defineField(i, 15))
    (1 to 5).filter(y => y != 2).foreach(y => defineBlockStone(8, y))
    defineBlockStone(6, 7)
    defineBlockStone(10, 7)
    (0 to 16).filter(i => i % 4 == 0).foreach(x => defineBlockStone(x, 11))
    definePlayerStones(1, player1)
    definePlayerStones(13, player4)
    if (playerCount >= 3) {
      definePlayerStones(5, player2)
      if (playerCount == 4) definePlayerStones(9, player3)
    }

    def defineField(x: Int, y: Int): Unit = board.update((x, y), Field(x, y, None))
    def defineBlockStone(x: Int, y: Int): Unit = board.update((x, y), Field(x, y, Some(BlockStone())))
    def definePlayerStones(xFirst: Int, player: Player): Unit = {
      board.update((xFirst, 14), Field(xFirst, 14, Some(PlayerStone(xFirst, 14, xFirst, 14, player.color))))
      board.update((xFirst, 15), Field(xFirst, 15, Some(PlayerStone(xFirst, 15, xFirst, 15, player.color))))
      board.update((xFirst + 1, 14), Field(xFirst + 1, 14, Some(PlayerStone(xFirst + 1, 14, xFirst + 1, 14, player.color))))
      board.update((xFirst + 2, 14), Field(xFirst + 2, 14, Some(PlayerStone(xFirst + 2, 14, xFirst + 2, 14, player.color))))
      board.update((xFirst + 2, 15), Field(xFirst + 2, 15, Some(PlayerStone(xFirst + 2, 15, xFirst + 2, 15, player.color))))
    }
    this
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

  def moveStone(current: Field, dest: Field): Option[Stone] =
    if (validField(dest.x, dest.y) && board((dest.x, dest.y)).available) {
      val save = dest.stone
      board((dest.x, dest.y)) = board((dest.x, dest.y)).copy(stone = Some(current.stone.get.asInstanceOf[PlayerStone].copy(x = dest.x, y = dest.y)))
      board((current.x, current.y)) = board((current.x, current.y)).copy(stone = None)
      save
    } else {
      None
    }


  def forceMoveStone(current: Field, dest: Field): Unit =
    if (board.contains((dest.x, dest.y))) {
      val ps: PlayerStone = current.stone.get.asInstanceOf[PlayerStone]
      board((dest.x, dest.y)) = board((dest.x, dest.y)).copy(stone = Some(ps.copy(x = dest.x, y = dest.y)))
      board((current.x, current.y)) = board((current.x, current.y)).copy(stone = None)
    }


  def resetPlayerStone(stone: PlayerStone): Unit = board((stone.startX, stone.startY)) =
    board((stone.startX, stone.startY)).copy(stone = Some(stone.copy(x = stone.startX, y = stone.startY)))

  def checkDestForBlockStone(x: Int, y: Int): Boolean = y < 12 && board.contains((x, y)) && board((x, y)).stone.isEmpty && (x, y) != (8, 0)

  def setBlockStoneOnField(field: Field): Unit = board((field.x, field.y)) = board((field.x, field.y)).copy(stone = Some(BlockStone()))

  def removeStoneOnField(field: Field): Unit = board((field.x, field.y)) = board((field.x, field.y)).copy(stone = None)

  def markPossibleMoves(stone: PlayerStone, player: Player, diced: Int): Unit = {
    if (stone.isOnStart) {
      markPossibleMovesR(player.start._1, player.start._2, diced, ' ', player.color)
    } else {
      markPossibleMovesR(stone.x, stone.y, diced, ' ', player.color)
    }
  }

  private def markPossibleMovesR(x: Int, y: Int, depth: Int, cameFrom: Char, playerColor: Int): Unit = {
    if (depth == 0) {
      //Dont hit your own kind
      if (board((x, y)).stone.isDefined && board((x, y)).stone.get.isInstanceOf[PlayerStone] && board((x, y)).stone.get.asInstanceOf[PlayerStone].playerColor == playerColor) {
        return
      }

      board((x, y)) = board((x, y)).copy(available = true)
    } else {
      // If there is a blocking stone on the way dont go on
      if (board((x, y)).stone.isDefined && board((x, y)).stone.get.isInstanceOf[BlockStone]) {
        return
      }
      // up
      if (validField(x, y - 1) && cameFrom != 'u') {
        markPossibleMovesR(x, y - 1, depth - 1, 'd', playerColor)
      }
      // down
      if (validField(x, y + 1) && cameFrom != 'd') {
        markPossibleMovesR(x, y + 1, depth - 1, 'u', playerColor)
      }
      // left
      if (validField(x - 1, y) && cameFrom != 'r') {
        markPossibleMovesR(x - 1, y, depth - 1, 'l', playerColor)
      }
      // right
      if (validField(x + 1, y) && cameFrom != 'l') {
        markPossibleMovesR(x + 1, y, depth - 1, 'r', playerColor)
      }
    }
  }

  def unmarkPossibleMoves(): Unit = {
    val tmpB = board.seq
    tmpB.foreach(f => board(f._1) = board(f._1).copy(available = false))
  }

  private def validField(x: Int, y: Int): Boolean = y <= 13 && board.contains((x, y))

  def checkDestForPlayerStone(x: Int, y: Int): Boolean = validField(x, y) && board((x, y)).available

  //wenn ein Stein im Zielfeld steht muss es ein Spielerstein sein => Sieg
  def checkWin: Boolean = board((8, 0)).stone.isDefined

}