package de.htwg.se.malefiz.controller

import de.htwg.se.malefiz.util.Command
import de.htwg.se.malefiz.model.gameboard.{ Field, FreeStone, PlayerStone, Stone }

class MoveCommand(stone: PlayerStone, destField: Field, controller: ControllerInterface) extends Command {

  private val xStone = stone.actualField.x
  private val yStone = stone.actualField.y
  private val currentField = controller.gameBoard.board(xStone)(yStone).get
  private var hitStone = new Stone('f')

  override def doStep(): Unit = {

    hitStone = controller.gameBoard.moveStone(currentField, destField).get

    hitStone.sort match {
      case 'p' => controller.gameBoard.resetPlayerStone(hitStone.asInstanceOf[PlayerStone])
      case 'f' =>
      case 'b' => controller.needToSetBlockStone = true
    }

    controller.gameBoard.unmarkPossibleMoves()
  }

  override def undoStep(): Unit = {

    controller.gameBoard.forceMoveStone(destField, currentField)

    destField.stone = hitStone

    if (hitStone.sort == 'p') {
      val x = hitStone.asInstanceOf[PlayerStone].startField.x
      val y = hitStone.asInstanceOf[PlayerStone].startField.y
      controller.gameBoard.board(x)(y).get.stone = FreeStone()
      hitStone.asInstanceOf[PlayerStone].actualField = destField
    }

    controller.gameBoard.markPossibleMoves(stone, controller.activePlayer, controller.diced)

    controller.needToSetBlockStone = false
  }

  override def redoStep(): Unit = doStep()

}
