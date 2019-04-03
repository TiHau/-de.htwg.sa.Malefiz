package de.htwg.se.malefiz.model.gameboard

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class StoneSpec extends WordSpec with Matchers {
  "A PlayerStone" when {
    "new" should {
      val stone: PlayerStone = PlayerStone(Field(0, 0, null), Field(0, 0, null), 1)
      "have a Player" in {
        stone.playerColor should be(1)
      }
      "have a startField" in {
        stone.startField should be(Field(0, 0, null))
      }
      "have stonetype p" in {
        stone.isInstanceOf[PlayerStone] shouldBe true
      }
      stone.actualField = Field(1, 1, null)

      "have a acctualField after change" in {
        stone.actualField should be(Field(1, 1, null))
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
