package de.htwg.se.malefiz.controller

import de.htwg.se.malefiz.util.Command
import de.htwg.se.malefiz.model.gameboard.{ BlockStone, Field, PlayerStone, Stone }

class MoveCommand(stone: PlayerStone, destField: Field, controller: ControllerInterface) extends Command {

  private val xStone = stone.x
  private val yStone = stone.y
  private val currentField = controller.gameBoard.board((xStone, yStone))
  private var hitStone: Option[Stone] = None

  override def doStep(): Unit = {

    hitStone = controller.gameBoard.moveStone(currentField, destField)

    hitStone match {
      case Some(stone: PlayerStone) => controller.gameBoard.resetPlayerStone(stone)
      case Some(_: BlockStone) => controller.needToSetBlockStone = true
      case _ =>
    }

    controller.gameBoard.unmarkPossibleMoves()
  }

  override def undoStep(): Unit = {

    controller.gameBoard.forceMoveStone(destField, currentField)

    destField.stone = hitStone

    hitStone match {
      case Some(stone: PlayerStone) => {
        val x = stone.startX
        val y = stone.startY
        controller.gameBoard.board((x, y)).stone = None
        stone.x = destField.x
        stone.y = destField.y
      }
      case _ =>
    }

    controller.gameBoard.markPossibleMoves(stone, controller.activePlayer, controller.diced)

    controller.needToSetBlockStone = false
  }

  override def redoStep(): Unit = doStep()

}
