package de.htwg.se.malefiz.controller

import com.google.inject.{ Guice, Injector }
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.controller.State._
import de.htwg.se.malefiz.model.gameboard._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.concurrent.Await

@RunWith(classOf[JUnitRunner])
class ControllerSpec extends WordSpec with Matchers {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  "A Controller" when {
    "new set" should {
      val controller = injector.getInstance(classOf[ControllerInterface])
      val player = Player(1, (2, 14))
      "can change player count specific" in {
        controller.newGame(2) equals controller.gameBoard.playerCount
      }

    }
  }
  "A Controller" when {
    "reset" should {
      val controller = injector.getInstance(classOf[ControllerInterface])
      "can reset" in {
        controller.reset()
        controller.getState shouldBe SetPlayerCount
      }

    }
  }

  "A Controller" when {
    "started" should {
      val controller = injector.getInstance(classOf[ControllerInterface])
      "take input" in {
        controller.takeInput(3, 14)
        controller.getState shouldBe Print
      }
      "redo does nothing" in {
        controller.takeInput(3, 14)
        controller.redo()
        controller.getState shouldBe Print
      }

      "chose Player" in {
        controller.setState(ChoosePlayerStone)
        controller.diced = 1
        controller.takeInput(10, 14)
        controller.undo()
        controller.getState shouldBe ChoosePlayerStone
        controller.undo()
        controller.getState shouldBe ChoosePlayerStone
        controller.redo()
        controller.getState shouldBe ChooseTarget
      }
      "choose Target" in {
        controller.takeInput(10, 13)
        controller.undo()
        controller.getState shouldBe ChooseTarget
        controller.redo()
        controller.getState shouldBe BeforeEndOfTurn
      }
      "chose Player not start" in {
        controller.setState(ChoosePlayerStone)
        controller.diced = 1
        controller.takeInput(10, 13)
        controller.undo()
        controller.getState shouldBe ChoosePlayerStone
        controller.redo()
        controller.getState shouldBe ChooseTarget
      }
      "choose Target not start" in {
        controller.takeInput(11, 13)
        controller.undo()
        controller.getState shouldBe ChooseTarget
        controller.redo()
        controller.getState shouldBe BeforeEndOfTurn
      }
      "before end of turn" in {
        controller.setState(BeforeEndOfTurn)
        controller.takeInput(3, 14)
        controller.getState shouldBe BeforeEndOfTurn
      }
      "end move" in {
        controller.endTurn()
        controller.getState shouldBe ChoosePlayerStone
      }
      "print" in {
        controller.setState(Print)
        controller.takeInput(3, 14)
        controller.getState shouldBe Print
      }
      "setBlockStone" in {
        controller.setState(SetBlockStone)
        controller.takeInput(4, 1)
        controller.undo()
        controller.getState shouldBe SetBlockStone
        controller.redo()
        controller.getState shouldBe BeforeEndOfTurn
      }
      "befor end of turn" in {
        controller.setState(BeforeEndOfTurn)
        controller.takeInput(4, 1)
        controller.getState shouldBe BeforeEndOfTurn
      }

      "change player from 3 to 4" in {
        controller.setState(BeforeEndOfTurn)
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player4
      }
      "change player from 4 to 2" in {
        controller.setState(BeforeEndOfTurn)
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player2
      }
      "change player from 1 to 3" in {
        controller.setState(BeforeEndOfTurn)
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player3
      }
      "change player from 2 to 1" in {
        controller.setState(BeforeEndOfTurn)
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player1
      }
      "change player from 4 to 1 with Playercount = 2" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.activePlayer = controller.gameBoard.player4
        controller.setState(BeforeEndOfTurn)
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player1

      }
      "check win" in {
        //        controller.gameBoard.board((8, 0)) = controller.gameBoard.board((8, 0)).copy(stone = Some(PlayerStone(8, 0, 8, 0, 1)))
        controller.setState(BeforeEndOfTurn)
        controller.endTurn()
        controller.getState shouldBe PlayerWon
      }
      "invalid PlayerStone" in {
        controller.setState(ChoosePlayerStone)
        controller.takeInput(13, 13)
        controller.getState shouldBe ChoosePlayerStone
      }
      "beat a PlayerStone" in {
        controller.newGame(2)
        Thread.sleep(10)
        val field1 = controller.gameBoard.board((14, 14))
        val field2 = controller.gameBoard.board((14, 13))
        controller.activePlayer = controller.gameBoard.player1
        controller.diced = 1
        controller.setState(ChoosePlayerStone)
        controller.takeInput(2, 14)
        controller.setState(ChooseTarget)
        controller.gameBoard.forceMoveStone(field1, field2)

        //  controller.gameBoard.board((14, 13)) = controller.gameBoard.board((14, 13)).copy(available = true)
        controller.takeInput(14, 13)
        controller.undo()
        controller.getState shouldBe ChooseTarget
      }

      "beat a BlockStone" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.activePlayer = controller.gameBoard.player1
        controller.diced = 5
        controller.takeInput(2, 14)
        controller.takeInput(4, 11)
        controller.gameBoard.board((4, 11)).stone.get.isInstanceOf[PlayerStone] shouldBe true

        controller.undo()
        controller.gameBoard.board((4, 11)).stone.get.isInstanceOf[BlockStone] shouldBe true
        controller.redo()
        controller.gameBoard.board((4, 11)).stone.get.isInstanceOf[PlayerStone] shouldBe true
      }

      "undo before end of turn" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.activePlayer = controller.gameBoard.player1
        controller.diced = 1
        controller.setState(ChoosePlayerStone)
        controller.takeInput(3, 14)
        controller.getState shouldBe ChooseTarget
        controller.takeInput(2, 13)
        controller.getState shouldBe BeforeEndOfTurn
        controller.redo()
        controller.getState shouldBe BeforeEndOfTurn
        controller.undo()
        controller.getState shouldBe ChooseTarget
        controller.redo()
        controller.getState shouldBe BeforeEndOfTurn
      }

      "set invalid playerstone target" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.setState(ChooseTarget)
        controller.takeInput(0, 15)
        controller.getState shouldBe ChooseTarget
      }

      "set invalid bockstone target" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.setState(SetBlockStone)
        controller.takeInput(0, 15)
        controller.getState shouldBe SetBlockStone
      }

      "input in state beforeEndOfTurn" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.setState(BeforeEndOfTurn)
        controller.takeInput(0, 15)
        controller.getState shouldBe BeforeEndOfTurn
      }

      "input while state is print" in {
        controller.newGame(2)
        Thread.sleep(10)
        controller.setState(Print)
        controller.takeInput(2, 14)
        controller.getState shouldBe Print
      }

      "when a new Games with 3 Players starts" in {
        controller.newGame(3)
        Thread.sleep(10)
        controller.getState shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player4
        controller.needToSetBlockStone shouldBe false
      }

      "when a new Game with 4 Players starts" in {
        controller.newGame(4)
        Thread.sleep(10)
        controller.getState shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when a new playerCount of 2 is set" in {
        controller.setPlayerCount(2)
        Thread.sleep(10)
        controller.getState shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when a new playerCount of 3 is set" in {
        controller.setPlayerCount(3)
        Thread.sleep(10)
        controller.getState shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when a new playerCount of 4 is set" in {
        controller.setPlayerCount(4)
        Thread.sleep(10)
        controller.getState shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when input comes in in SetPlayerCount State" in {
        controller.newGame(4)
        Thread.sleep(10)
        controller.setState(SetPlayerCount)
        controller.takeInput(0, 0)
        controller.getState shouldBe SetPlayerCount
      }

      "when input comes in in EndTurn state" in {
        controller.newGame(4)
        Thread.sleep(10)
        controller.setState(EndTurn)
        controller.takeInput(0, 0)
        controller.getState shouldBe EndTurn
      }

      "when input comes in in PlayerWon state" in {
        controller.newGame(4)
        Thread.sleep(100)
        controller.setState(PlayerWon)
        controller.takeInput(0, 0)
        controller.getState shouldBe PlayerWon
      }

      "getter/setter tests" in {
        controller.newGame(4)
        Thread.sleep(10)
        val testField = Field(8, 0, None)
        controller.setDestField(testField)
        controller.getDestField shouldBe testField

        val testPlayerStone = controller.gameBoard.board(controller.gameBoard.player1.start).stone.get.asInstanceOf[PlayerStone]
        controller.setChoosenPlayerStone(testPlayerStone)
        controller.getChoosenPlayerStone shouldBe testPlayerStone
      }

      "save and load" in {
        controller.newGame(4)
        Thread.sleep(10)
        val oldDiced: Int = controller.diced
        val oldPlayer: Player = controller.activePlayer
        controller.saveGame()
        controller.loadSavedGame()
        controller.diced shouldBe oldDiced
        controller.activePlayer shouldBe oldPlayer
      }

    }
  }

}

