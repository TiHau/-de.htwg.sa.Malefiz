package de.htwg.se.malefiz.controller

import de.htwg.se.malefiz.util.Command
import de.htwg.se.malefiz.model.gameboard.Field

class BlockStoneCommand(field: Field, controller: ControllerInterface) extends Command {

  override def doStep(): Unit = controller.setGameBoad(controller.gameBoard.setBlockStoneOnField(field))

  override def undoStep(): Unit = {
    controller.setGameBoad(controller.gameBoard.removeStoneOnField(field))
    controller.needToSetBlockStone = true
  }

  override def redoStep(): Unit = doStep()

}
