package de.htwg.se.malefiz.model

import com.google.inject.{Guice, Injector}
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.controller.ControllerInterface
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class FileIOSpec extends WordSpec with Matchers {

 /* "A FileIO Json" when {

    val injector: Injector = Guice.createInjector(new MalefizModule)
    val controller: ControllerInterface = injector.getInstance(classOf[ControllerInterface])
    "save load" should {
      controller.activePlayer = controller.gameBoard.player3
      controller.saveGame()
      Thread.sleep(10)
      controller.activePlayer = controller.gameBoard.player1
      controller.loadSavedGame()
      "have player nr 3" in {
        controller.activePlayer.color shouldBe 3
      }

    }

  }*/
}
