package de.htwg.se.malefiz.model.gameboard

import com.google.inject.Inject
import com.google.inject.name.Named

import scala.collection.mutable
import scala.swing.Publisher

case class GameBoard @Inject() (@Named("DefaultSize") var playerCount: Int) extends GameBoardInterface with Publisher {

  override def createBoard: GameBoard = {
    (0 to 15).foreach {
      case y @ (0 | 4) => defineField(8, y)
      case y @ (1 | 3) => (0 to 16).foreach(i => defineField(i, y))
      case y @ 2 =>
        defineField(0, y)
        defineField(16, y)
      case y @ 5 => (6 to 10).foreach(i => defineField(i, y))
      case y @ 6 =>
        defineField(6, y)
        defineField(10, y)
      case y @ 7 => (4 to 12).foreach(i => defineField(i, y))
      case y @ 8 =>
        defineField(12, y)
        defineField(4, y)
      case y @ 9 => (2 to 14).foreach(i => defineField(i, y))
      case y @ 10 =>
        defineField(2, y)
        defineField(6, y)
        defineField(10, y)
        defineField(14, y)
      case y @ (11 | 13) => (0 to 16).foreach(i => defineField(i, y))
      case y @ 12 => (0 to 16).filter(i => i % 4 == 0).foreach(i => defineField(i, y))
      case y @ 14 => (1 to 15).filter(i => i % 4 != 0).foreach(i => defineField(i, y))
      case y @ 15 => (0 to 16).filter(i => i % 2 != 0).foreach(i => defineField(i, y))
    }
    (1 to 5).filter(y => y != 2).foreach(y => defineBlockStone(8, y))
    defineBlockStone(6, 7)
    defineBlockStone(10, 7)
    (0 to 16).filter(i => i % 4 == 0).foreach(x => defineBlockStone(x, 11))
    definePlayerStoneArray(1, player1)
    definePlayerStoneArray(5, player2)
    definePlayerStoneArray(9, player3)
    definePlayerStoneArray(13, player4)
    definePlayerStones(1, player1)
    definePlayerStones(13, player4)
    if (playerCount >= 3) {
      definePlayerStones(5, player2)
      if (playerCount == 4) definePlayerStones(9, player3)
    }
    def definePlayerStoneArray(xStart: Int, player: Player): Unit = {
      player.stones(2) = PlayerStone(xStart, 14, xStart, 14, player.color)
      player.stones(1) = PlayerStone(xStart, 15, xStart, 15, player.color)
      player.stones(0) = PlayerStone(xStart + 1, 14, xStart + 1, 14, player.color)
      player.stones(3) = PlayerStone(xStart + 2, 14, xStart + 2, 14, player.color)
      player.stones(4) = PlayerStone(xStart + 2, 15, xStart + 2, 15, player.color)
    }
    def defineField(x: Int, y: Int): Unit = board((x, y)) = Field(x, y, None)
    def defineBlockStone(x: Int, y: Int): Unit = board((x, y)) = Field(x, y, Some(BlockStone()))
    def definePlayerStones(xFirst: Int, player: Player): Unit = {
      board((xFirst, 14)) = Field(xFirst, 14, Some(player.stones(2)))
      board((xFirst, 15)) = Field(xFirst, 15, Some(player.stones(1)))
      board((xFirst + 1, 14)) = Field(xFirst + 1, 14, Some(player.stones(0)))
      board((xFirst + 2, 14)) = Field(xFirst + 2, 14, Some(player.stones(3)))
      board((xFirst + 2, 15)) = Field(xFirst + 2, 15, Some(player.stones(4)))
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
              if (s.avariable) {
                stone.playerColor match {
                  case 1 => jsb.append("|G|")
                  case 2 => jsb.append("|H|")
                  case 3 => jsb.append("|J|")
                  case 4 => jsb.append("|K|")
                  case _ => jsb.append("|P|")
                }
              } else jsb.append("|" + stone.playerColor + "|")
            case Some(_: BlockStone) =>
              if (s.avariable) jsb.append("|B|") else jsb.append("|-|")
            case None =>
              if (s.avariable) jsb.append("|x|") else jsb.append("|o|")
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

  def moveStone(current: Field, dest: Field): Option[Stone] = {
    if (validField(dest.x, dest.y) && board((dest.x, dest.y)).avariable) {
      val save = dest.stone
      dest.stone = current.stone
      dest.stone.get.asInstanceOf[PlayerStone].x = dest.x
      dest.stone.get.asInstanceOf[PlayerStone].y = dest.y
      current.stone = None
      save
    } else {
      None
    }
  }

  def forceMoveStone(current: Field, dest: Field): Unit = {
    if (dest.y < 16 && dest.y >= 0 && dest.x < 17 && dest.x >= 0) {
      val ps: PlayerStone = current.stone.get.asInstanceOf[PlayerStone]
      if (dest.x == ps.startX && dest.y == ps.startY) {
        ps.x = ps.startX
        ps.y = ps.startY
      } else {
        ps.x = dest.x
        ps.y = dest.y
      }
      dest.stone = current.stone
      current.stone = None
    }
  }

  def resetPlayerStone(stone: PlayerStone): Unit = {
    stone.x = stone.startX
    stone.y = stone.startY
    board((stone.startX, stone.startY)).stone = Some(stone)
  }

  def checkDestForBlockStone(x: Int, y: Int): Boolean = y < 12 && y > 0 && x < 17 && x >= 0 && board((x, y)).stone.isEmpty

  def setBlockStoneOnField(field: Field): Unit = board((field.x, field.y)).stone = Some(new BlockStone)

  def removeStoneOnField(field: Field): Unit = board((field.x, field.y)).stone = None

  def markPossibleMoves(stone: PlayerStone, player: Player, diced: Int): Unit = {
    if (stone.isOnStart) {
      markPossibleMovesR(player.stones(0).startX, player.stones(0).startY, diced, ' ', player.color)
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
      board((x, y)) = board((x, y)).copy(avariable = true)
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
    tmpB.foreach(f => board(f._1) = board(f._1).copy(avariable = false))
  }

  private def validField(x: Int, y: Int): Boolean = y <= 13 && y >= 0 && x <= 16 && x >= 0 && board.contains((x, y))

  def checkDestForPlayerStone(x: Int, y: Int): Boolean = validField(x, y) && board((x, y)).avariable

  //wenn ein Stein im Zielfeld steht muss es ein Spielerstein sein => Sieg
  def checkWin: Boolean = board((8, 0)).stone.isDefined

}