package de.htwg.se.malefiz.model.gameboard
import com.google.inject.name.Names
import com.google.inject.{Guice, Injector}
import de.htwg.se.malefiz.MalefizModule
import net.codingwell.scalaguice.InjectorExtensions._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class GameBoardSpec extends WordSpec with Matchers {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  var board: GameBoardInterface = _
  "A GameBoard" when {
    "initialized" should {
      board = injector.instance[GameBoardInterface](Names.named("default")).createBoard
      "have no field with null" in {
        for (y <- 0 to 15) {
          for (x <- 0 to 16) {
            if (board.board.contains((x, y))) {
              board.board((x, y)) shouldNot be(null)
            }
          }
        }
      }
    }
  }
  "A GameBoard" when {
    "seted all" should {

      board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "have 20 Player Stones" in {
        var count = 0
        for (y <- 0 to 15) {
          for (x <- 0 to 16) {
            if (board.board.contains((x, y))) {
              val field = board.board((x, y))
              if (field.stone.isDefined && field.stone.get.isInstanceOf[PlayerStone]) {
                count += 1
              }
            }
          }
        }
        count shouldBe (20)
      }
    }
  }
  "A GameBoard" when {
    "toSting" should {
      board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "return a  which is not empty" in {

        board.toString().isEmpty shouldBe (false)
      }
    }
  }

  "A GameBoard" when {
    "not win" should {
      board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "should return false" in {

        board.checkWin.shouldBe(false)
      }
    }
  }

  "A GameBoard" when {
    "move Stone" should {
      board = injector.instance[GameBoardInterface](Names.named("default")).createBoard
      "Returns Option None" in {
        val tmp = board.moveStone(board.board((3, 14)), board.board((8, 0)))
        tmp._1 should be(None)
      }
    }
  }

}

