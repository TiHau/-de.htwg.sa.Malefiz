package de.htwg.se.malefiz.model.fileio.fileioJson

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import de.htwg.se.malefiz.controller.ControllerInterface
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import de.htwg.se.malefiz.model.gameboard._
import play.api.libs.json._

import scala.io.Source

class FileIO extends FileIOInterface {
  override def load(controller: ControllerInterface): Unit = {
    if (Files.exists(Paths.get("saveFile.json"))) {
      val source: String = Source.fromFile("saveFile.json").getLines.mkString
      val json: JsValue = Json.parse(source)
      gameFromJson(json, controller)
    }
  }

  private def gameFromJson(json: JsValue, controller: ControllerInterface): Unit = {
    controller.setPlayerCount((json \ "playerCount").get.toString().toInt)
    controller.diced = (json \ "diced").get.toString().toInt

    (json \ "activePlayer").get.toString().toInt match {
      case 1 => controller.activePlayer = controller.gameBoard.player1
      case 2 => controller.activePlayer = controller.gameBoard.player2
      case 3 => controller.activePlayer = controller.gameBoard.player3
      case _ => controller.activePlayer = controller.gameBoard.player4
    }

    val cb = controller.gameBoard.board.seq
    cb.foreach(f=>if(controller.gameBoard.board(f._1).isDefined)controller.gameBoard.board(f._1).get.stone = None)

    (json \ "blockStones").as[JsArray].value.foreach(blockStone => {
      val x = (blockStone \ "x").get.toString().toInt
      val y = (blockStone \ "y").get.toString().toInt
      controller.gameBoard.board((x,y)).get.stone = Some(BlockStone())
    })

    (json \ "playerStones").as[JsArray].value.foreach(playerStone => {
      val x = (playerStone \ "x").get.toString().toInt
      val y = (playerStone \ "y").get.toString().toInt
      val startX = (playerStone \ "startX").get.toString().toInt
      val startY = (playerStone \ "startY").get.toString().toInt
      val playerColor = (playerStone \ "playerColor").get.toString().toInt

      playerColor match {
        case 1 =>
          controller.gameBoard.player1.stones.filter(stone => stone.startX == startX && stone.startY == startY).foreach(stone => {
            stone.x = x
            stone.y = y
            controller.gameBoard.board((x,y)).get.stone = Some(stone)
          })
        case 2 =>
          controller.gameBoard.player2.stones.filter(stone => stone.startX == startX && stone.startY == startY).foreach(stone => {
            stone.x = x
            stone.y = y
            controller.gameBoard.board((x, y)).get.stone = Some(stone)
          })
        case 3 =>
          controller.gameBoard.player3.stones.filter(stone => stone.startX == startX && stone.startY == startY).foreach(stone => {
            stone.x = x
            stone.y = y
            controller.gameBoard.board((x, y)).get.stone = Some(stone)
          })
        case 4 =>
          controller.gameBoard.player4.stones.filter(stone => stone.startX == startX && stone.startY == startY).foreach(stone => {
            stone.x = x
            stone.y = y
            controller.gameBoard.board((x, y)).get.stone = Some(stone)
          })
        case _ =>
      }
    })
  }

  override def save(controller: ControllerInterface): Unit = {
    val pw = new PrintWriter(new File("saveFile.json"))
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
          (0 to 13) filter (y => controller.gameBoard.board((x,y)).isDefined)
            filter (y => controller.gameBoard.board((x,y)).get.stone.isDefined)
            filter (y => controller.gameBoard.board((x,y)).get.stone.get.isInstanceOf[BlockStone])
            map (y => Json.obj("x" -> JsNumber(x), "y" -> JsNumber(y))))),
      "playerStones" -> Json.toJson(
        (0 to 16) flatMap (x =>
          (0 to 15) filter (y => controller.gameBoard.board((x,y)).isDefined)
            filter (y => controller.gameBoard.board((x,y)).get.stone.isDefined)
            filter (y => controller.gameBoard.board((x,y)).get.stone.get.isInstanceOf[PlayerStone])
            map (y => Json.obj(
            "x" -> JsNumber(x),
            "y" -> JsNumber(y),
            "startX" -> JsNumber(controller.gameBoard.board((x, y)).get.stone.get.asInstanceOf[PlayerStone].startX),
            "startY" -> JsNumber(controller.gameBoard.board((x, y)).get.stone.get.asInstanceOf[PlayerStone].startY),
            "playerColor" -> JsNumber(controller.gameBoard.board((x, y)).get.stone.get.asInstanceOf[PlayerStone].playerColor))))))
  }
}