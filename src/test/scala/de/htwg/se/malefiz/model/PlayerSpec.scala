package de.htwg.se.malefiz.model.gameboard

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PlayerSpec extends WordSpec with Matchers {
  "A Player" when {
    "new" should {
      val player = Player(1, (2, 14))
      "have a name" in {
        player.color should be(1)
      }

    }
  }
}
