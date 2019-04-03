package de.htwg.se.malefiz.model.gameboard
import com.google.inject.name.Names
import com.google.inject.{ Guice, Injector }
import net.codingwell.scalaguice.InjectorExtensions._
import de.htwg.se.malefiz.MalefizModule
import org.junit.runner.RunWith
import org.scalatest.{ Matchers, WordSpec }
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardSpec extends WordSpec with Matchers {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  "A GameBoard" when {
    "initialized" should {
      val board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "have empty space" in {
        board.board(0)(0).isEmpty should be(true)
      }
      "have no field with null" in {
        for (y <- 0 to 15) {
          for (x <- 0 to 16) {
            board.board(x)(y) shouldNot be(null)
          }
        }
      }
    }
  }
  "A GameBoard" when {
    "seted all" should {
      val board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "have 20 Player Stones" in {
        var count = 0
        for (y <- 0 to 15) {
          for (x <- 0 to 16) {
            if (board.board(x)(y).isDefined) {
              val field = board.board(x)(y).get
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
      val board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "return a  which is not empty" in {

        board.toString().isEmpty shouldBe (false)
      }
    }
  }

  "A GameBoard" when {
    "not win" should {
      val board = injector.instance[GameBoardInterface](Names.named("default")).createBoard

      "should return false" in {

        board.checkWin.shouldBe(false)
      }
    }
  }

  "A GameBoard" when {
    "move Stone" should {
      val board = injector.instance[GameBoardInterface](Names.named("default")).createBoard
      "Returns Option None" in {
        board.moveStone(board.board(3)(14).get, board.board(8)(0).get) should be(None)
      }
    }
  }

}

