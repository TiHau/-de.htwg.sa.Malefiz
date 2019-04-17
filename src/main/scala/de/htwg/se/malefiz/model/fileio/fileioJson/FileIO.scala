package de.htwg.se.malefiz.model.fileio.fileioJson

import java.io.{ File, PrintWriter }
import java.nio.file.{ Files, Paths }

import de.htwg.se.malefiz.controller.ControllerInterface
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import de.htwg.se.malefiz.model.gameboard._
import play.api.libs.json._

import scala.io.Source

class FileIO extends FileIOInterface {
  private val source: String = "saveFile.json"
  override def load(controller: ControllerInterface): Unit = {
    if (Files.exists(Paths.get("saveFile.json"))) {
      val json: JsValue = Json.parse(Source.fromFile(source).getLines.mkString)
      gameFromJson(json, controller)
    }
  }

  private def gameFromJson(json: JsValue, controller: ControllerInterface): Unit = {
    controller.setPlayerCount((json \ "playerCount").get.toString().toInt)
    controller.diced = (json \ "diced").get.toString().toInt

    controller.activePlayer = (json \ "activePlayer").get.toString().toInt match {
      case 1 => controller.gameBoard.player1
      case 2 => controller.gameBoard.player2
      case 3 => controller.gameBoard.player3
      case _ => controller.gameBoard.player4
    }

    val cb = controller.gameBoard.board.seq
    //   cb.foreach(f => controller.gameBoard.board(f._1) = controller.gameBoard.board(f._1).copy(stone = None))

    (json \ "blockStones").as[JsArray].value.foreach(blockStone => {
      val x = (blockStone \ "x").get.toString().toInt
      val y = (blockStone \ "y").get.toString().toInt
      //  controller.gameBoard.board((x, y)) = controller.gameBoard.board((x, y)).copy(stone = Some(BlockStone()))
    })

    (json \ "playerStones").as[JsArray].value.foreach(playerStone => {
      val x = (playerStone \ "x").get.toString().toInt
      val y = (playerStone \ "y").get.toString().toInt
      val startX = (playerStone \ "startX").get.toString().toInt
      val startY = (playerStone \ "startY").get.toString().toInt
      val playerColor = (playerStone \ "playerColor").get.toString().toInt
      //      controller.gameBoard.board((x, y)) = controller.gameBoard.board((x, y)).copy(stone = Some(PlayerStone(startX, startY, x, y, playerColor)))
    })
  }

  override def save(controller: ControllerInterface): Unit = {
    val pw = new PrintWriter(new File(source))
    pw.write(Json.prettyPrint(gameToJson(controller)))
    pw.close()
  }

  def gameToJson(controller: ControllerInterface): JsObject = {
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