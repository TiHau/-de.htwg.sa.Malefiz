package de.htwg.se.malefiz.model.gameboard

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StoneFieldSpec extends WordSpec with Matchers {
  "A Field" when {
    "new" should {
      val field: Field = Field(0, 0, Some(PlayerStone(0, 0, 0, 0, 1)))

      "have a x coordinate" in {
        field.x should be(0)
      }
      "have a y coordinate" in {
        field.y should be(0)
      }
      "have a stone" in {
        field.stone.get.isInstanceOf[PlayerStone] shouldBe true
      }
      "avariable false" in {
        field.avariable shouldBe false
      }
      "is empty false" in {
        field.stone.isEmpty shouldBe false

      }

    }
  }
}
