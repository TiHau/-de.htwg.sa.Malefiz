package de.htwg.se.malefiz.util

import de.htwg.se.malefiz.model.gameboard.{BlockStone, GameBoardInterface, PlayerStone}
import de.htwg.se.malefiz.Malefiz.controller
import de.htwg.se.malefiz.controller.State
import play.api.libs.json._


object JsonConverter {

  def gameToJson(): JsObject = {

    Json.obj(
      "activePlayer" -> JsNumber(controller.activePlayer.color),
      "diced" -> JsString(controller.diced.toString),
      "message" -> JsString(message),
      "rows" -> Json.toJson(
        for {
          y <- 0 to 15
        } yield rowToJson(controller.gameBoard, y)))

  }
  def rowToJson(gameBoard: GameBoardInterface, y: Int): JsObject = {
    Json.obj(
      "rowNr" -> JsNumber(y),
      "fields" -> Json.toJson(
        for {
          x <- 0 to 16
        } yield fieldToJson(gameBoard, x, y)))
  }
  def fieldToJson(gameBoard: GameBoardInterface, x: Int, y: Int): JsObject = {
    if (gameBoard.board.contains((x, y))) {
      val field = gameBoard.board((x, y))
      field.stone match {
        case Some(stone: PlayerStone) =>
          Json.obj(
            "isFreeSpace" -> JsBoolean(false),
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "sort" -> JsString(stone.playerColor.toString),
            "avariable" -> JsBoolean(field.available))
        case Some(_: BlockStone) =>
          Json.obj(
            "isFreeSpace" -> JsBoolean(false),
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "sort" -> JsString("b"),
            "avariable" -> JsBoolean(field.available))

        case None =>
          Json.obj(
            "isFreeSpace" -> JsBoolean(false),
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "sort" -> JsString("f"),
            "avariable" -> JsBoolean(field.available))
      }
    } else {
      Json.obj(
        "isFreeSpace" -> JsBoolean(true),
        "x" -> JsNumber(x),
        "y" -> JsNumber(y),
        "sort" -> JsString("f"),
        "avariable" -> JsBoolean(false))
    }
  }
  def message: String = {
    controller.getState match {
      case State.SetBlockStone =>
        "Set a BlockStone"

      case State.ChoosePlayerStone =>
        "Chose one of your Stones"

      case State.ChooseTarget =>
        "Chose a Target Field"

      case State.BeforeEndOfTurn =>
        "End turn"

      case State.SetPlayerCount =>
        "Start a Game"

      case State.PlayerWon =>
        "Victory"

      case _ => "Start a Game"
    }
  }


}
