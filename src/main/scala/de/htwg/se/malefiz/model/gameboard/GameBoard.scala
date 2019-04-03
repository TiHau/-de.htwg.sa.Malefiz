package de.htwg.se.malefiz.model.gameboard

import com.google.inject.Inject
import com.google.inject.name.Named

import scala.collection.mutable
import scala.swing.Publisher

case class GameBoard @Inject()(@Named("DefaultSize") var playerCount: Int) extends GameBoardInterface with Publisher {
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

  private def buildMalefitzGameBoard(board: Array[Array[Option[Field]]]): Array[Array[Option[Field]]] = {
    for (y <- nu0 to nu15) {
      for (x <- nu0 to nu16) {
        board(x)(y) = None
      }
    }
    for (y <- nu0 to nu15) {

      y match {
        case 0 => board(nu8)(y) = Some(Field(nu8, y, None))
        case 1 => for (i <- nu0 to nu16) {
          board(i)(y) = Some(Field(i, y, None))
        }
        case 2 =>
          board(nu0)(y) = Some(Field(nu0, y, None))
          board(nu16)(y) = Some(Field(nu16, y, None))
        case 3 => for (i <- nu0 to nu16)
          board(i)(y) = Some(Field(i, y, None))
        case 4 => board(nu8)(y) = Some(Field(nu8, y, None))
        case 5 => for (i <- nu0 to nu16)
          if (i >= nu6 && i <= nu10) {
            board(i)(y) = Some(Field(i, y, None))
          }
        case 6 =>
          board(nu6)(y) = Some(Field(nu6, y, None))
          board(nu10)(y) = Some(Field(nu10, y, None))

        case 7 => for (i <- nu0 to nu16) {
          if (i >= nu4 && i <= nu12) {
            board(i)(y) = Some(Field(i, y, None))
          }
        }
        case 8 =>
          board(nu12)(y) = Some(Field(nu12, y, None))
          board(nu4)(y) = Some(Field(nu4, y, None))

        case 9 => for (i <- nu0 to nu16) {
          if (i >= nu2 && i <= nu14) {
            board(i)(y) = Some(Field(i, y, None))
          }
        }
        case 10 =>
          board(nu2)(y) = Some(Field(nu2, y, None))
          board(nu6)(y) = Some(Field(nu6, y, None))
          board(nu10)(y) = Some(Field(nu10, y, None))
          board(nu14)(y) = Some(Field(nu14, y, None))

        case 11 => for (i <- nu0 to nu16) {
          board(i)(y) = Some(Field(i, y, None))
        }
        case 12 => for (i <- nu0 to nu16) {
          if (i % nu4 == nu0) {
            board(i)(y) = Some(Field(i, y, None))
          }
        }
        case 13 => for (i <- nu0 to nu16) {
          board(i)(y) = Some(Field(i, y, None))
        }
        case 14 => for (i <- nu0 to nu16) {
          if ((i >= nu1 && i <= nu3) || (i >= nu5 && i <= nu7) || (i >= nu9 && i <= nu11) || (i >= nu13 && i <= nu15)) {
            board(i)(y) = Some(Field(i, y, None))
          }
        }
        case 15 => for (i <- nu0 to nu16) {
          if (!(i % nu2 == nu0)) {
            board(i)(y) = Some(Field(i, y, None))
          }
        }

      }
    }
    board
  }

  override def toString: String = {
    val jsb = new mutable.StringBuilder()
    for (y <- nu0 to nu15) {
      if (y < 10) {
        jsb.append(y + "  ")
      } else {
        jsb.append(y + " ")
      }

      for (i <- nu0 to nu16) {
        if (board(i)(y).isEmpty) {
          jsb.append("   ")
        } else {
          val s: Field = board(i)(y).get

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
              } else {
                jsb.append("|" + stone.playerColor + "|")
              }
            case Some(_: BlockStone) =>
              if (s.avariable) {
                jsb.append("|B|")
              } else {
                jsb.append("|-|")
              }
            case None =>
              if (s.avariable) {
                jsb.append("|x|")
              } else {
                jsb.append("|o|")
              }
            case _ =>
          }
        }
      }
      jsb.append("\n")
    }
    jsb.append("    ")
    (0 to 9).addString(jsb, "  ")
    jsb.append(" ")
    (10 to 16).addString(jsb, " ")
    jsb.toString()
  }

  private def setBlockStones(board: Array[Array[Option[Field]]]): Array[Array[Option[Field]]] = {
    board(nu8)(nu1) = Some(Field(nu8, nu1, Some(BlockStone())))
    board(nu8)(nu3) = Some(Field(nu8, nu3, Some(BlockStone())))
    board(nu8)(nu4) = Some(Field(nu8, nu4, Some(BlockStone())))
    board(nu8)(nu5) = Some(Field(nu8, nu5, Some(BlockStone())))
    board(nu6)(nu7) = Some(Field(nu6, nu7, Some(BlockStone())))
    board(nu10)(nu7) = Some(Field(nu10, nu7, Some(BlockStone())))
    board(nu0)(nu11) = Some(Field(nu0, nu11, Some(BlockStone())))
    board(nu4)(nu11) = Some(Field(nu4, nu11, Some(BlockStone())))
    board(nu8)(nu11) = Some(Field(nu8, nu11, Some(BlockStone())))
    board(nu12)(nu11) = Some(Field(nu12, nu11, Some(BlockStone())))
    board(nu16)(nu11) = Some(Field(nu16, nu11, Some(BlockStone())))
    board
  }

  private def setPlayerStones(board: Array[Array[Option[Field]]], playerCount: Int): Array[Array[Option[Field]]] = {

    board(nu1)(nu14) = Some(Field(nu1, nu14, Some(player1.stones(nu2))))
    board(nu1)(nu15) = Some(Field(nu1, nu15, Some(player1.stones(nu1))))
    board(nu2)(nu14) = Some(Field(nu2, nu14, Some(player1.stones(nu0))))
    board(nu3)(nu14) = Some(Field(nu3, nu14, Some(player1.stones(nu3))))
    board(nu3)(nu15) = Some(Field(nu3, nu15, Some(player1.stones(nu4))))

    board(nu13)(nu14) = Some(Field(nu13, nu14, Some(player4.stones(nu2))))
    board(nu13)(nu15) = Some(Field(nu13, nu15, Some(player4.stones(nu1))))
    board(nu14)(nu14) = Some(Field(nu14, nu14, Some(player4.stones(nu0))))
    board(nu15)(nu14) = Some(Field(nu15, nu14, Some(player4.stones(nu3))))
    board(nu15)(nu15) = Some(Field(nu15, nu15, Some(player4.stones(nu4))))

    if (playerCount >= 3) {

      board(nu5)(nu14) = Some(Field(nu5, nu14, Some(player2.stones(nu2))))
      board(nu5)(nu15) = Some(Field(nu5, nu15, Some(player2.stones(nu1))))
      board(nu6)(nu14) = Some(Field(nu6, nu14, Some(player2.stones(nu0))))
      board(nu7)(nu14) = Some(Field(nu7, nu14, Some(player2.stones(nu3))))
      board(nu7)(nu15) = Some(Field(nu7, nu15, Some(player2.stones(nu4))))

      if (playerCount == 4) {

        board(nu10)(nu14) = Some(Field(nu10, nu14, Some(player3.stones(nu0))))
        board(nu9)(nu15) = Some(Field(nu9, nu15, Some(player3.stones(nu1))))
        board(nu9)(nu14) = Some(Field(nu9, nu14, Some(player3.stones(nu2))))
        board(nu11)(nu14) = Some(Field(nu11, nu14, Some(player3.stones(nu3))))
        board(nu11)(nu15) = Some(Field(nu11, nu15, Some(player3.stones(nu4))))
      }
    }

    board
  }

  def moveStone(current: Field, dest: Field): Option[Stone] = {
    if (validField(dest.x, dest.y) && board(dest.x)(dest.y).get.avariable) {
      val save = dest.stone
      dest.stone = current.stone
      dest.stone.get.asInstanceOf[PlayerStone].actualField = dest
      current.stone = None
      save
    } else {
      None
    }
  }

  def forceMoveStone(current: Field, dest: Field): Unit = {
    if (dest.y > 15 || dest.y < 0 || dest.x > 16 || dest.x < 0) {
    } else {

      if (dest == current.stone.get.asInstanceOf[PlayerStone].startField) {
        current.stone.get.asInstanceOf[PlayerStone].actualField = current.stone.get.asInstanceOf[PlayerStone].startField
      } else {
        current.stone.get.asInstanceOf[PlayerStone].actualField = dest
      }

      dest.stone = current.stone
      current.stone = None
    }
  }

  def resetPlayerStone(stone: PlayerStone): Unit = {
    stone.actualField = stone.startField
    val x = stone.startField.x
    val y = stone.startField.y
    board(x)(y).get.stone = Some(stone)
  }

  def checkDestForBlockStone(x: Int, y: Int): Boolean = y < 12 && y > 0 && x < 17 && x >= 0 && board(x)(y).get.stone.isEmpty

  def setBlockStoneOnField(field: Field): Unit = board(field.x)(field.y).get.stone = Some(new BlockStone)

  def removeStoneOnField(field: Field): Unit = board(field.x)(field.y).get.stone = None

  def markPossibleMoves(stone: PlayerStone, player: Player, diced: Int): Unit = {
    if (stone.actualField == stone.startField) {
      val x = player.stones(0).startField.x
      val y = player.stones(0).startField.y
      markPossibleMovesR(x, y, diced, ' ', player.color)
    } else {
      val x = stone.actualField.x
      val y = stone.actualField.y
      markPossibleMovesR(x, y, diced, ' ', player.color)
    }
  }

  private def markPossibleMovesR(x: Int, y: Int, depth: Int, cameFrom: Char, playerColor: Int): Unit = {
    if (depth == 0) {
      //Dont hit your own kind
      if (board(x)(y).get.stone.isDefined && board(x)(y).get.stone.get.isInstanceOf[PlayerStone] && board(x)(y).get.stone.get.asInstanceOf[PlayerStone].playerColor == playerColor) {
        return
      }
      board(x)(y).get.avariable = true
    } else {
      // If there is a blocking stone on the way dont go on
      if (board(x)(y).get.stone.isDefined && board(x)(y).get.stone.get.isInstanceOf[BlockStone]) {
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

  def unmarkPossibleMoves(): Unit = (0 to 15).map(y => (0 to 16).map(x => if (board(x)(y).isDefined) board(x)(y).get.avariable = false))

  private def validField(x: Int, y: Int): Boolean = y <= 13 && y >= 0 && x <= 16 && x >=0 && board(x)(y).isDefined

  def checkDestForPlayerStone(x: Int, y: Int): Boolean = validField(x,y) && board(x)(y).get.avariable

  //wenn ein Stein im Zielfeld steht muss es ein Spielerstein sein => Sieg
  def checkWin: Boolean = board(nu8)(nu0).get.stone.isDefined

  override def createBoard: GameBoard = {
    if (playerCount > 4) {
      playerCount = nu4
    } else if (playerCount < 2) {
      playerCount = nu2
    }
    buildMalefitzGameBoard(board)
    setBlockStones(board)

    player1.stones(nu2) = PlayerStone(board(nu1)(nu14).get, board(nu1)(nu14).get, player1.color)
    player1.stones(nu1) = PlayerStone(board(nu1)(nu15).get, board(nu1)(nu15).get, player1.color)
    player1.stones(nu0) = PlayerStone(board(nu2)(nu14).get, board(nu2)(nu14).get, player1.color)
    player1.stones(nu3) = PlayerStone(board(nu3)(nu14).get, board(nu3)(nu14).get, player1.color)
    player1.stones(nu4) = PlayerStone(board(nu3)(nu15).get, board(nu3)(nu15).get, player1.color)

    player2.stones(nu2) = PlayerStone(board(nu5)(nu14).get, board(nu5)(nu14).get, player2.color)
    player2.stones(nu1) = PlayerStone(board(nu5)(nu15).get, board(nu5)(nu15).get, player2.color)
    player2.stones(nu0) = PlayerStone(board(nu6)(nu14).get, board(nu6)(nu14).get, player2.color)
    player2.stones(nu3) = PlayerStone(board(nu7)(nu14).get, board(nu7)(nu14).get, player2.color)
    player2.stones(nu4) = PlayerStone(board(nu7)(nu15).get, board(nu7)(nu15).get, player2.color)

    player3.stones(nu2) = PlayerStone(board(nu9)(nu14).get, board(nu9)(nu14).get, player3.color)
    player3.stones(nu1) = PlayerStone(board(nu9)(nu15).get, board(nu9)(nu15).get, player3.color)
    player3.stones(nu0) = PlayerStone(board(nu10)(nu14).get, board(nu10)(nu14).get, player3.color)
    player3.stones(nu3) = PlayerStone(board(nu11)(nu14).get, board(nu11)(nu14).get, player3.color)
    player3.stones(nu4) = PlayerStone(board(nu11)(nu15).get, board(nu11)(nu15).get, player3.color)

    player4.stones(nu2) = PlayerStone(board(nu13)(nu14).get, board(nu13)(nu14).get, player4.color)
    player4.stones(nu1) = PlayerStone(board(nu13)(nu15).get, board(nu13)(nu15).get, player4.color)
    player4.stones(nu0) = PlayerStone(board(nu14)(nu14).get, board(nu14)(nu14).get, player4.color)
    player4.stones(nu3) = PlayerStone(board(nu15)(nu14).get, board(nu15)(nu14).get, player4.color)
    player4.stones(nu4) = PlayerStone(board(nu15)(nu15).get, board(nu15)(nu15).get, player4.color)

    setPlayerStones(board, playerCount)
    this
  }
}
