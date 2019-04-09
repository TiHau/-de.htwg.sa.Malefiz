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
        Files.exists(Paths.get("saveFile.json")) shouldBe false
      }


    }
  }
}
