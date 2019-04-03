package de.htwg.se.malefiz.model.fileio.fileioxml

import java.nio.file.{ Files, Paths }

import de.htwg.se.malefiz.controller.{ ControllerInterface, State }
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import de.htwg.se.malefiz.model.gameboard._

import scala.xml.NodeSeq

class FileIO extends FileIOInterface {
  override def load(controller: ControllerInterface): Unit = {
    if (Files.exists(Paths.get("saveFile.xml"))) {
      val file = xml.XML.loadFile("saveFile.xml")
      val boardNode: NodeSeq = file \\ "board"
      loadBoard(boardNode, controller)
      val controllerNode = file \\ "controller"
      loadController(controllerNode, controller)

      controller.notifyObservers()
    }
  }

  private def loadBoard(boardNode: NodeSeq, controller: ControllerInterface): Unit = {
    val playerCount = boardNode \\ "playerCount"
    controller.setPlayerCount(playerCount.text.trim.toInt)

    val fieldNodes = boardNode \\ "field"
    for (fieldNode <- fieldNodes) {
      val x = (fieldNode \\ "x").text.trim.toInt
      val y = (fieldNode \\ "y").text.trim.toInt
      controller.gameBoard.board(x)(y).get.avariable = (fieldNode \\ "avariable").text.trim.toBoolean

      (fieldNode \\ "sort").text.trim.last match {
        case 'p' =>

          val startFieldX = (fieldNode \\ "startFieldX").text.trim.toInt
          val startFieldY = (fieldNode \\ "startFieldY").text.trim.toInt

          val playerStones =
            controller.gameBoard.player1.stones ++
              controller.gameBoard.player2.stones ++
              controller.gameBoard.player3.stones ++
              controller.gameBoard.player4.stones

          for (playerStone <- playerStones) {
            if (playerStone.startField.x == startFieldX && playerStone.startField.y == startFieldY) {
              playerStone.actualField = controller.gameBoard.board(x)(y).get
              controller.gameBoard.board(x)(y).get.stone = Some(playerStone)
            }
          }
        case 'b' =>
          controller.gameBoard.board(x)(y).get.stone = Some(BlockStone())
        case 'f' =>
          controller.gameBoard.board(x)(y).get.stone = None
      }
    }
  }

  private def loadController(controllerNode: NodeSeq, controller: ControllerInterface): Unit = {
    controller.needToSetBlockStone = (controllerNode \\ "needToSetBlockStone").text.trim.toBoolean
    controller.diced = (controllerNode \\ "diced").text.trim.toInt

    controller.activePlayer = (controllerNode \\ "activePlayer").text.trim.toInt match {
      case 1 =>
        controller.gameBoard.player1
      case 2 =>
        controller.gameBoard.player2
      case 3 =>
        controller.gameBoard.player3
      case 4 =>
        controller.gameBoard.player4
    }
    controller.state = State.fromString((controllerNode \\ "state").text.trim).get

    val startFieldX = (controllerNode \\ "choosenPlayerStone" \ "startX").text.trim.toInt
    val startFieldY = (controllerNode \\ "choosenPlayerStone" \ "startY").text.trim.toInt
    val playerStones =
      controller.gameBoard.player1.stones ++
        controller.gameBoard.player2.stones ++
        controller.gameBoard.player3.stones ++
        controller.gameBoard.player4.stones

    for (playerStone <- playerStones) {
      if (playerStone.startField.x == startFieldX && playerStone.startField.y == startFieldY) {
        controller.setChoosenPlayerStone(playerStone)
      }
    }

    controller.setDestField(controller.gameBoard.board(
      (controllerNode \\ "destField" \ "x").text.trim.toInt)(
        (controllerNode \\ "destField" \ "y").text.trim.toInt).get)
  }

  override def save(controller: ControllerInterface): Unit = {
    xml.XML.save("saveFile.xml", gameToXml(controller))
  }

  private def gameToXml(controller: ControllerInterface) = {
    <game>
      <controller>
        <activePlayer>
          { controller.activePlayer.color }
        </activePlayer>
        <diced>
          { controller.diced }
        </diced>
        <state>
          { controller.state }
        </state>
        <choosenPlayerStone>
          <startX>
            { controller.getChoosenPlayerStone.startField.x }
          </startX>
          <startY>
            { controller.getChoosenPlayerStone.startField.x }
          </startY>
        </choosenPlayerStone>
        <destField>
          <x>
            { controller.getDestField.x }
          </x>
          <y>
            { controller.getDestField.y }
          </y>
        </destField>
        <needToSetBlockStone>
          { controller.needToSetBlockStone }
        </needToSetBlockStone>
      </controller>{ boardToXml(controller.gameBoard) }
    </game>

  }

  private def boardToXml(board: GameBoardInterface) = {
    <board>
      <playerCount>
        { board.playerCount }
      </playerCount>{
        for {
          x <- 0 to 16
          y <- 0 to 15
        } yield fieldToXml(board, x, y)
      }
    </board>
  }

  private def fieldToXml(board: GameBoardInterface, x: Int, y: Int) = {
    if (board.board(x)(y).isDefined) {
      val field = board.board(x)(y).get
      field.stone match {
        case Some(stone: PlayerStone) =>
          <field>
            <x>
              { x }
            </x>
            <y>
              { y }
            </y>
            <sort>
              { "p" }
            </sort>
            <avariable>
              { field.avariable }
            </avariable>
            <startFieldX>
              { stone.startField.x }
            </startFieldX>
            <startFieldY>
              { stone.startField.y }
            </startFieldY>
          </field>
        case Some(_:BlockStone) =>
          <field>
            <x>
              { x }
            </x>
            <y>
              { y }
            </y>
            <sort>
              { "b" }
            </sort>
            <avariable>
              { field.avariable }
            </avariable>
          </field>
        case _ =>
          <field>
            <x>
              { x }
            </x>
            <y>
              { y }
            </y>
            <sort>
              { "f" }
            </sort>
            <avariable>
              { field.avariable }
            </avariable>
          </field>
      }
    }
  }
}
