package de.htwg.se.malefiz.model.gameboard

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StoneSpec extends WordSpec with Matchers {
  "A PlayerStone" when {
    "new" should {
      val stone: PlayerStone = PlayerStone(0, 0, 0, 0, 1)
      "have a Player" in {
        stone.playerColor should be(1)
      }
      "have a startField" in {
        stone.startX shouldBe 0
        stone.startY shouldBe 0
      }
      "have stonetype p" in {
        stone.isInstanceOf[PlayerStone] shouldBe true
      }
      val stone2 = stone.copy(x = 1, y = 1)

      "have a acctualField after change" in {
        stone2.x shouldBe 1
        stone2.y shouldBe 1
      }

    }
  }
  "A BlockStone" when {
    "new" should {
      val stone: BlockStone = BlockStone()

      "have stonetype b" in {
        stone.isInstanceOf[BlockStone] shouldBe true
      }
    }
  }
  "A Stone" when {
    "new" should {
      val stone: Stone = new Stone()

      "have stonetype ' '" in {
        stone.isInstanceOf[Stone] shouldBe true
      }

    }
  }
}
