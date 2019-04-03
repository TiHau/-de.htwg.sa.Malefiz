package de.htwg.se.malefiz.controller

import com.google.inject.{Guice, Injector}
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.controller.State._
import de.htwg.se.malefiz.model.gameboard._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ControllerSpec extends WordSpec with Matchers {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  "A Controller" when {
    "new set" should {
      val controller = injector.getInstance(classOf[ControllerInterface])
      val player = Player(1)
      "can change player count specific" in {
        controller.newGame(2) equals (controller.gameBoard.playerCount)
      }

    }
  }
  "A Controller" when {
    "reset" should {
      val controller = injector.getInstance(classOf[ControllerInterface])
      "can reset" in {
        controller.reset()
        controller.state shouldBe SetPlayerCount
      }

    }
  }

  "A Controller" when {
    "started" should {
      val controller = injector.getInstance(classOf[ControllerInterface])
      "when set Target" in {
        val ret = controller.setTargetForPlayerStone(3, 14)
        ret shouldBe false
      }
      "take input" in {
        controller.takeInput(3, 14)
        controller.state shouldBe Print
      }
      "redo does nothing" in {
        controller.takeInput(3, 14)
        controller.redo()
        controller.state shouldBe Print
      }

      "chose Player" in {
        controller.state = ChoosePlayerStone
        controller.diced = 1
        controller.takeInput(10, 14)
        controller.undo()
        controller.state shouldBe ChoosePlayerStone
        controller.undo()
        controller.state shouldBe ChoosePlayerStone
        controller.redo()
        controller.state shouldBe ChooseTarget
      }
      "choose Target" in {
        controller.takeInput(10, 13)
        controller.undo()
        controller.state shouldBe ChooseTarget
        controller.redo()
        controller.state shouldBe BeforeEndOfTurn
      }
      "chose Player not start" in {
        controller.state = ChoosePlayerStone
        controller.diced = 1
        controller.takeInput(10, 13)
        controller.undo()
        controller.state shouldBe ChoosePlayerStone
        controller.redo()
        controller.state shouldBe ChooseTarget
      }
      "choose Target not start" in {
        controller.takeInput(11, 13)
        controller.undo()
        controller.state shouldBe ChooseTarget
        controller.redo()
        controller.state shouldBe BeforeEndOfTurn
      }
      "before end of turn" in {
        controller.state = BeforeEndOfTurn
        controller.takeInput(3, 14)
        controller.state shouldBe BeforeEndOfTurn
      }
      "end move" in {
        controller.endTurn()
        controller.state shouldBe ChoosePlayerStone
      }
      "print" in {
        controller.state = Print
        controller.takeInput(3, 14)
        controller.state shouldBe Print
      }
      "setBlockStone" in {
        controller.state = SetBlockStone
        controller.takeInput(4, 1)
        controller.undo()
        controller.state shouldBe SetBlockStone
        controller.redo()
        controller.state shouldBe BeforeEndOfTurn
      }
      "befor end of turn" in {
        controller.state = BeforeEndOfTurn
        controller.takeInput(4, 1)
        controller.state shouldBe BeforeEndOfTurn
      }

      "change player from 3 to 4" in {
        controller.state = BeforeEndOfTurn
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player4
      }
      "change player from 4 to 2" in {
        controller.state = BeforeEndOfTurn
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player2
      }
      "change player from 1 to 3" in {
        controller.state = BeforeEndOfTurn
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player3
      }
      "change player from 2 to 1" in {
        controller.state = BeforeEndOfTurn
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player1
      }
      "change player from 4 to 1 with Playercount = 2" in {
        controller.newGame(2)
        controller.activePlayer = controller.gameBoard.player4
        controller.state = BeforeEndOfTurn
        controller.endTurn()
        controller.activePlayer shouldBe controller.gameBoard.player1

      }
      "check win" in {
        controller.gameBoard.board(8)(0).get.stone = Some(PlayerStone(controller.gameBoard.board(8)(0).get, controller.gameBoard.board(8)(0).get, 1))
        controller.state = BeforeEndOfTurn
        controller.endTurn()
        controller.state shouldBe PlayerWon
      }
      "invalid PlayerStone" in {
        controller.state = ChoosePlayerStone
        controller.takeInput(0, 0)
        controller.state shouldBe ChoosePlayerStone
      }
      "beat a PlayerStone" in {
        controller.newGame(2)
        val field1 = controller.gameBoard.board(14)(14).get
        val field2 = controller.gameBoard.board(14)(13).get
        controller.activePlayer = controller.gameBoard.player1
        controller.diced = 1
        controller.state = ChoosePlayerStone
        controller.takeInput(2, 14)
        controller.state = ChooseTarget
        controller.gameBoard.forceMoveStone(field1, field2)
        controller.gameBoard.board(14)(13).get.avariable = true
        controller.takeInput(14, 13)
        controller.undo()
        controller.state shouldBe ChooseTarget
      }

      "beat a BlockStone" in {
        controller.newGame(2)
        controller.activePlayer = controller.gameBoard.player1
        controller.diced = 5
        controller.takeInput(2, 14)
        controller.takeInput(4, 11)
        controller.gameBoard.board(4)(11).get.stone.get.isInstanceOf[PlayerStone] shouldBe true

        controller.undo()
        controller.gameBoard.board(4)(11).get.stone.get.isInstanceOf[BlockStone] shouldBe true
        controller.redo()
        controller.gameBoard.board(4)(11).get.stone.get.isInstanceOf[PlayerStone] shouldBe true
      }

      "undo before end of turn" in {
        controller.newGame(2)
        controller.activePlayer = controller.gameBoard.player1
        controller.diced = 1
        controller.state = ChoosePlayerStone
        controller.takeInput(3, 14)
        controller.state shouldBe ChooseTarget
        controller.takeInput(2, 13)
        controller.state shouldBe BeforeEndOfTurn
        controller.redo()
        controller.state shouldBe BeforeEndOfTurn
        controller.undo()
        controller.state shouldBe ChooseTarget
        controller.redo()
        controller.state shouldBe BeforeEndOfTurn
      }

      "set invalid playerstone target" in {
        controller.newGame(2)
        controller.state = ChooseTarget
        controller.takeInput(0, 15)
        controller.state shouldBe ChooseTarget
      }

      "set invalid bockstone target" in {
        controller.newGame(2)
        controller.state = SetBlockStone
        controller.takeInput(0, 15)
        controller.state shouldBe SetBlockStone
      }

      "input in state beforeEndOfTurn" in {
        controller.newGame(2)
        controller.state = BeforeEndOfTurn
        controller.takeInput(0, 15)
        controller.state shouldBe BeforeEndOfTurn
      }

      "input while state is print" in {
        controller.newGame(2)
        controller.state = Print
        controller.takeInput(2, 14)
        controller.state shouldBe Print
      }

      "when a new Games with 3 Players starts" in {
        controller.newGame(3)
        controller.state shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player4
        controller.needToSetBlockStone shouldBe false
      }

      "when a new Game with 4 Players starts" in {
        controller.newGame(4)
        controller.state shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when a new playerCount of 2 is set" in {
        controller.setPlayerCount(2)
        controller.state shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when a new playerCount of 3 is set" in {
        controller.setPlayerCount(3)
        controller.state shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when a new playerCount of 4 is set" in {
        controller.setPlayerCount(4)
        controller.state shouldBe ChoosePlayerStone
        controller.activePlayer shouldBe controller.gameBoard.player2
        controller.needToSetBlockStone shouldBe false
      }

      "when input comes in in SetPlayerCount State" in {
        controller.newGame(4)
        controller.state = SetPlayerCount
        controller.takeInput(0,0)
        controller.state shouldBe SetPlayerCount
      }

      "when input comes in in EndTurn state" in {
        controller.newGame(4)
        controller.state = EndTurn
        controller.takeInput(0,0)
        controller.state shouldBe EndTurn
      }


      "when input comes in in PlayerWon state" in {
        controller.newGame(4)
        controller.state = PlayerWon
        controller.takeInput(0,0)
        controller.state shouldBe PlayerWon
      }


      "getter/setter tests" in {
        controller.newGame(4)
        val testField = Field(8,0,None)
        controller.setDestField(testField)
        controller.getDestField shouldBe testField

        val testPlayerStone = controller.gameBoard.player1.stones(0)
        controller.setChoosenPlayerStone(testPlayerStone)
        controller.getChoosenPlayerStone shouldBe testPlayerStone
      }

      "save and load" in {
        controller.newGame(4)
        val oldState: State.Value = controller.state
        val oldDiced: Int = controller.diced
        val oldPlayer: Player = controller.activePlayer
        controller.saveGame()
        controller.loadSavedGame()
        controller.state shouldBe oldState
        controller.diced shouldBe oldDiced
        controller.activePlayer shouldBe oldPlayer
      }

    }
  }

}

