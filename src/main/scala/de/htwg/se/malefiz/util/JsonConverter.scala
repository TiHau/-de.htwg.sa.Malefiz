package de.htwg.se.malefiz.util

import de.htwg.se.malefiz.model.gameboard.{BlockStone, PlayerStone}
import de.htwg.se.malefiz.Malefiz.controller
import play.api.libs.json._


object JsonConverter {

  def gameToJson(): JsObject = {

      Json.obj(
        "activePlayer" -> JsNumber(controller.activePlayer.color),
        "diced" -> JsNumber(controller.diced),
        "playerCount" -> JsNumber(controller.gameBoard.playerCount),
        "blockStones" -> Json.toJson(
          (0 to 16) flatMap (x =>
            (0 to 13) filter (y => controller.gameBoard.board.contains((x, y)))
              filter (y => controller.gameBoard.board((x, y)).stone.isDefined)
              filter (y => controller.gameBoard.board((x, y)).stone.get.isInstanceOf[BlockStone])
              map (y => Json.obj("x" -> JsNumber(x), "y" -> JsNumber(y))))),
        "playerStones" -> Json.toJson(
          (0 to 16) flatMap (x =>
            (0 to 15) filter (y => controller.gameBoard.board.contains((x, y)))
              filter (y => controller.gameBoard.board((x, y)).stone.isDefined)
              filter (y => controller.gameBoard.board((x, y)).stone.get.isInstanceOf[PlayerStone])
              map (y => Json.obj(
              "x" -> JsNumber(x),
              "y" -> JsNumber(y),
              "startX" -> JsNumber(controller.gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone].startX),
              "startY" -> JsNumber(controller.gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone].startY),
              "playerColor" -> JsNumber(controller.gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone].playerColor))))))

  }


}
