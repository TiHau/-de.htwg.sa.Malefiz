package de.htwg.se.malefiz.model.gameboard

import com.google.inject.Inject
import com.google.inject.name.Named

import scala.collection.mutable
import scala.swing.Publisher

case class GameBoard @Inject() (@Named("DefaultSize") var playerCount: Int) extends GameBoardInterface with Publisher {
  private val nu0 = 0
  private val nu1 = 1
  private val nu2 = 2
  private val nu3 = 3
  private val nu4 = 4
  private val nu5 = 5
  private val nu6 = 6
  private val nu7 = 7
  private val nu8 = 8
  private val nu9 = 9
  private val nu10 = 10
  private val nu11 = 11
  private val nu12 = 12
  private val nu13 = 13
  private val nu14 = 14
  private val nu15 = 15
  private val nu16 = 16
  override def createBoard: GameBoard = {
    (nu0 to nu15).foreach(y => (nu0 to nu16).foreach(x => board +=((x,y)-> None)))
    (nu0 to nu15).foreach {
      case y @ (0 | 4) => defineField(nu8, y)
      case y @ (1 | 3) => (nu0 to nu16).foreach(i => defineField(i, y))
      case y @ 2 =>
        defineField(nu0, y)
        defineField(nu16, y)
      case y @ 5 => (nu6 to nu10).foreach(i => defineField(i, y))
      case y @ 6 =>
        defineField(nu6, y)
        defineField(nu10, y)
      case y @ 7 => (nu4 to nu12).foreach(i => defineField(i, y))
      case y @ 8 =>
        defineField(nu12, y)
        defineField(nu4, y)
      case y @ 9 => (nu2 to nu14).foreach(i => defineField(i, y))
      case y @ 10 =>
        defineField(nu2, y)
        defineField(nu6, y)
        defineField(nu10, y)
        defineField(nu14, y)
      case y @ (11 | 13) => (nu0 to nu16).foreach(i => defineField(i, y))
      case y @ 12 => (nu0 to nu16).filter(i => i % nu4 == nu0).foreach(i => defineField(i, y))
      case y @ 14 => (nu1 to nu15).filter(i => i % nu4 != nu0).foreach(i => defineField(i, y))
      case y @ 15 => (nu0 to nu16).filter(i => i % nu2 != nu0).foreach(i => defineField(i, y))
    }
    (1 to 5).filter(y => y != 2).foreach(y => defineBlockStone(nu8, y))
    defineBlockStone(nu6, nu7)
    defineBlockStone(nu10, nu7)
    (0 to 16).filter(i => i % 4 == 0).foreach(x => defineBlockStone(x, nu11))
    definePlayerStoneArray(nu1, player1)
    definePlayerStoneArray(nu5, player2)
    definePlayerStoneArray(nu9, player3)
    definePlayerStoneArray(nu13, player4)
    definePlayerStones(nu1, player1)
    definePlayerStones(nu13, player4)
    if (playerCount >= 3) {
      definePlayerStones(nu5, player2)
      if (playerCount == 4) definePlayerStones(nu9, player3)
    }
    def definePlayerStoneArray(xStart: Int, player: Player): Unit = {
      player.stones(nu2) = PlayerStone(xStart, nu14, xStart, nu14, player.color)
      player.stones(nu1) = PlayerStone(xStart, nu15, xStart, nu15, player.color)
      player.stones(nu0) = PlayerStone(xStart + 1, nu14, xStart + 1, nu14, player.color)
      player.stones(nu3) = PlayerStone(xStart + 2, nu14, xStart + 2, nu14, player.color)
      player.stones(nu4) = PlayerStone(xStart + 2, nu15, xStart + 2, nu15, player.color)
    }
    def defineField(x: Int, y: Int): Unit = board.update((x,y),Some(Field(x, y, None)))
    def defineBlockStone(x: Int, y: Int): Unit = board.update((x,y),Some(Field(x, y, Some(BlockStone()))))
    def definePlayerStones(xFirst: Int, player: Player): Unit = {
      board.update((xFirst,nu14), Some(Field(xFirst, nu14, Some(player.stones(nu2)))))
      board.update((xFirst,nu15), Some(Field(xFirst, nu15, Some(player.stones(nu1)))))
      board.update((xFirst + 1,nu14), Some(Field(xFirst + 1, nu14, Some(player.stones(nu0)))))
      board.update((xFirst + 2,nu14), Some(Field(xFirst + 2, nu14, Some(player.stones(nu3)))))
      board.update((xFirst + 2,nu15), Some(Field(xFirst + 2, nu15, Some(player.stones(nu4)))))
    }
    this
  }


  override def toString: String = {
    val jsb = new mutable.StringBuilder()
    (nu0 to nu15).foreach(y => {
      if (y < 10) jsb.append(y + "  ") else jsb.append(y + " ")
      (nu0 to nu16).foreach(i => {
        if (board((i,y)).isEmpty) jsb.append("   ")
        else {
          val s: Field = board((i,y)).get
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
    if (validField(dest.x, dest.y) && board((dest.x,dest.y)).get.avariable) {
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
    board((stone.startX, stone.startY)).get.stone = Some(stone)
  }

  def checkDestForBlockStone(x: Int, y: Int): Boolean = y < 12 && y > 0 && x < 17 && x >= 0 && board((x, y)).get.stone.isEmpty

  def setBlockStoneOnField(field: Field): Unit = board((field.x,field.y)).get.stone = Some(new BlockStone)

  def removeStoneOnField(field: Field): Unit = board((field.x,field.y)).get.stone = None

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
      if (board((x, y)).get.stone.isDefined && board((x, y)).get.stone.get.isInstanceOf[PlayerStone] && board((x,y)).get.stone.get.asInstanceOf[PlayerStone].playerColor == playerColor) {
        return
      }
      board((x, y)).get.avariable = true
    } else {
      // If there is a blocking stone on the way dont go on
      if (board((x,y)).get.stone.isDefined && board((x,y)).get.stone.get.isInstanceOf[BlockStone]) {
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
    tmpB.filter(f=>board(f._1).isDefined).foreach(f=> board(f._1).get.avariable = false)
  }

  private def validField(x: Int, y: Int): Boolean = y <= 13 && y >= 0 && x <= 16 && x >= 0 && board((x, y)).isDefined

  def checkDestForPlayerStone(x: Int, y: Int): Boolean = validField(x, y) && board((x, y)).get.avariable

  //wenn ein Stein im Zielfeld steht muss es ein Spielerstein sein => Sieg
  def checkWin: Boolean = board((nu8, nu0)).get.stone.isDefined


}