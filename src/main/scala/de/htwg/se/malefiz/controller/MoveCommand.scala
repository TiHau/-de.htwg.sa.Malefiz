package de.htwg.se.malefiz.controller

import de.htwg.se.malefiz.model.gameboard.{BlockStone, Field, PlayerStone, Stone}
import de.htwg.se.malefiz.util.Command

class MoveCommand(stone: PlayerStone, destField: Field, controller: ControllerInterface) extends Command {

  private val currentField = controller.gameBoard.board((stone.x, stone.y))
  private var hitStone: Option[Stone] = None

  override def doStep(): Unit = {
    val tmp = controller.gameBoard.moveStone(currentField, destField)
    controller.setGameBoad(tmp._2)
    hitStone = tmp._1
    hitStone match {
      case Some(stone: PlayerStone) => controller.setGameBoad(controller.gameBoard.resetPlayerStone(stone))
      case Some(_: BlockStone) => controller.needToSetBlockStone = true
      case _ =>
    }
    controller.setGameBoad(controller.gameBoard.unmarkPossibleMoves())
  }

  override def undoStep(): Unit = {
    controller.setGameBoad(controller.gameBoard.forceMoveStone(controller.gameBoard.board(destField.x, destField.y), currentField))
    controller.gameBoard.setField((destField.x, destField.y), controller.gameBoard.board((destField.x, destField.y)).copy(stone = hitStone))
    hitStone match {
      case Some(stone: PlayerStone) =>
        val x = stone.startX
        val y = stone.startY
        controller.gameBoard.setField((x, y), controller.gameBoard.board((x, y)).copy(stone = None))
        controller.gameBoard.setField(
          (destField.x, destField.y),
          controller.gameBoard.board((destField.x, destField.y)).copy(stone = Some(stone.copy(x = destField.x, y = destField.y))))

      case _ =>
    }
    controller.setGameBoad(controller.gameBoard.markPossibleMoves(stone, controller.activePlayer, controller.diced))
    controller.needToSetBlockStone = false
  }

  override def redoStep(): Unit = doStep()
}
