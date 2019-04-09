package de.htwg.se.malefiz.model

import java.nio.file.{Files, Paths}

import com.google.inject.{Guice, Injector}
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.controller.ControllerInterface
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class FileIOSpec extends WordSpec with Matchers {

  "A FileIO Json" when {
    val injector: Injector = Guice.createInjector(new MalefizModule)
    val controller: ControllerInterface = injector.getInstance(classOf[ControllerInterface])
    "gameStart" should {
      if (Files.exists(Paths.get("saveFile.json"))) {
        Files.delete(Paths.get("saveFile.json"))
      }

      "have no existing File" in {
        Files.exists(Paths.get("saveFile.json")) && Files.exists(Paths.get("saveFile.xml")) shouldBe false
      }

      "should can save and load then file exists and restore count with player 3" in {
        controller.setPlayerCount(2)
        controller.saveGame()
        controller.setPlayerCount(4)
        controller.loadSavedGame()
        Files.exists(Paths.get("saveFile.json")) shouldBe true
        controller.gameBoard.playerCount shouldBe 2

      }
      "should can save and load then file exists and restore count with player 2" in {
        controller.activePlayer = controller.gameBoard.player2
        controller.setPlayerCount(3)
        controller.saveGame()
        controller.setPlayerCount(4)
        controller.loadSavedGame()
        Files.exists(Paths.get("saveFile.json")) || Files.exists(Paths.get("saveFile.xml")) shouldBe true
        controller.gameBoard.playerCount shouldBe 3

      }
      "should can save and load then file exists and restore count with player 1" in {
        controller.activePlayer = controller.gameBoard.player1
        controller.setPlayerCount(2)
        controller.saveGame()
        controller.setPlayerCount(4)
        controller.loadSavedGame()
        Files.exists(Paths.get("saveFile.json")) || Files.exists(Paths.get("saveFile.xml")) shouldBe true
        controller.gameBoard.playerCount shouldBe 2

      }
      "should can save and load then file exists and restore count with player 4" in {
        controller.activePlayer = controller.gameBoard.player4
        controller.setPlayerCount(3)
        controller.saveGame()
        controller.setPlayerCount(4)
        controller.loadSavedGame()
        controller.gameBoard.playerCount shouldBe 3

      }
    }
  }
}
