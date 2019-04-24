package de.htwg.se.malefiz.controller

import de.htwg.se.malefiz.model.gameboard.PlayerStone
import de.htwg.se.malefiz.util.Command

class ChooseCommand(stone: PlayerStone, controller: ControllerInterface) extends Command {

  override def doStep(): Unit = controller.setGameBoad(controller.gameBoard.markPossibleMoves(stone, controller.activePlayer, controller.diced))

  override def undoStep(): Unit = controller.setGameBoad(controller.gameBoard.unmarkPossibleMoves())

  override def redoStep(): Unit = doStep()

}
