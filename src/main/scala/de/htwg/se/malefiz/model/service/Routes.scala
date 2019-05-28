package de.htwg.se.malefiz.model.service

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.google.inject.name.Names
import de.htwg.se.malefiz.model.gameboard.{BlockStone, Field, GameBoardInterface, PlayerStone}
import net.codingwell.scalaguice.InjectorExtensions._
import play.api.libs.json.{JsBoolean, JsNumber, JsValue, Json}


object Routes {

  val all: Route =
    get {
      path("") {
        complete {
          ""
        }
      } ~
        pathPrefix("new") {
          path(IntNumber) { playerCount =>
            complete {
              WebServer.gameBoard = playerCount match {
                case 2 => WebServer.injector.instance[GameBoardInterface](Names.named("tiny")).createBoard
                case 3 => WebServer.injector.instance[GameBoardInterface](Names.named("small")).createBoard
                case _ => WebServer.injector.instance[GameBoardInterface](Names.named("default")).createBoard
              }
              JsBoolean(true).toString()
            }
          }
        } ~
        path("playerCount") {
          complete {
            JsNumber(WebServer.gameBoard.playerCount).toString()
          }
        } ~
        pathPrefix("setBlockStoneOnField") {
          path(IntNumber / IntNumber) { (x, y) =>
            complete {
              val tmp = WebServer.gameBoard.setBlockStoneOnField(WebServer.gameBoard.board((x, y)))
              WebServer.gameBoard = tmp._2
              JsBoolean(tmp._1).toString()
            }
          }
        } ~
        pathPrefix("resetPlayerStone") {
          path(IntNumber / IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, startX, startY, color) =>
            complete {
              WebServer.gameBoard = WebServer.gameBoard.resetPlayerStone(PlayerStone(startX, startY, x, y, color))
              JsBoolean(true).toString()
            }
          }
        } ~
        pathPrefix("moveStone") {
          path(IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, xDest, yDest) =>
            complete {
              val start = WebServer.gameBoard.board((x, y))
              val dest = WebServer.gameBoard.board((xDest, yDest))
              val res = WebServer.gameBoard.moveStone(start, dest)
              val hitStone = res._2
              WebServer.gameBoard = res._3

              hitStone match {
                case Some(stone: PlayerStone) => Json.obj(
                  "moved" -> JsBoolean(res._1),
                  "sort" -> "p",
                  "x" -> JsNumber(stone.x),
                  "y" -> JsNumber(stone.y),
                  "startX" -> JsNumber(stone.startX),
                  "startY" -> JsNumber(stone.startY),
                  "color" -> JsNumber(stone.playerColor)).toString()
                case Some(_: BlockStone) => Json.obj("moved" -> JsBoolean(res._1), "sort" -> "b").toString()
                case _ => Json.obj("moved" -> JsBoolean(res._1), "sort" -> "f").toString()
              }
            }
          }
        } ~
        pathPrefix("markPossibleMoves") {
          path(IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, playerColor, diced) =>
            complete {
              if (WebServer.gameBoard.board.contains((x, y)) && !WebServer.gameBoard.board((x, y)).available) {
                WebServer.gameBoard = WebServer.gameBoard.unmarkPossibleMoves()
              }
              if (WebServer.gameBoard.board.contains((x, y))
                && WebServer.gameBoard.board((x, y)).stone.isDefined
                && WebServer.gameBoard.board((x, y)).stone.get.isInstanceOf[PlayerStone]
                && playerColor == WebServer.gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone].playerColor) {
                val stone = WebServer.gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone]
                val player = playerColor match {
                  case 1 => WebServer.gameBoard.player1
                  case 2 => WebServer.gameBoard.player2
                  case 3 => WebServer.gameBoard.player3
                  case _ => WebServer.gameBoard.player4
                }
                WebServer.gameBoard = WebServer.gameBoard.markPossibleMoves(stone, player, diced)
                JsBoolean(true).toString()
              } else {
                JsBoolean(false).toString()
              }
            }
          }
        } ~
        path("unmarkPossibleMoves") {
          complete {
            WebServer.gameBoard = WebServer.gameBoard.unmarkPossibleMoves()
            JsBoolean(true).toString()
          }
        } ~
        path("getString") {
          complete {
            WebServer.gameBoard.toString
          }
        } ~
        path("getJson") {
          complete {
            WebServer.gameBoard.toJson.toString
          }
        } ~
        pathPrefix("save") {
          path(IntNumber / IntNumber) { (activePlayer, diced) =>
            complete {
              WebServer.gameBoard.save(Json.obj(
                "activePlayer" -> JsNumber(activePlayer),
                "diced" -> JsNumber(diced)))
              JsBoolean(true).toString()
            }
          }
        } ~
        path("load") {
          complete {
            val (newGameBoard, activePlayer, diced) = WebServer.gameBoard.load()
            WebServer.gameBoard = newGameBoard
            Json.obj(
              "activePlayer" -> JsNumber(activePlayer),
              "diced" -> JsNumber(diced)).toString()
          }
        }
    }
}
