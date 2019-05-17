package de.htwg.se.malefiz.model.service

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.google.inject.name.Names
import de.htwg.se.malefiz.model.gameboard.{BlockStone, Field, GameBoardInterface, PlayerStone}
import net.codingwell.scalaguice.InjectorExtensions._
import play.api.libs.json.{JsBoolean, JsNumber, Json}


object Routes {

  val all: Route =
    get {
      path("") {
        complete {
          ""
        }
      } ~
        path("player1") {
          complete {
            Json.obj(
              "color" -> JsNumber(WebServer.gameBoard.player1.color),
              "xStart" -> JsNumber(WebServer.gameBoard.player1.start._1),
              "yStart" -> JsNumber(WebServer.gameBoard.player1.start._2)
            ).toString()
          }
        } ~
        path("player2") {
          complete {
            Json.obj(
              "color" -> JsNumber(WebServer.gameBoard.player2.color),
              "xStart" -> JsNumber(WebServer.gameBoard.player2.start._1),
              "yStart" -> JsNumber(WebServer.gameBoard.player2.start._2)
            ).toString()
          }
        } ~
        path("player3") {
          complete {
            Json.obj(
              "color" -> JsNumber(WebServer.gameBoard.player3.color),
              "xStart" -> JsNumber(WebServer.gameBoard.player3.start._1),
              "yStart" -> JsNumber(WebServer.gameBoard.player3.start._2)
            ).toString()
          }
        } ~
        path("player4") {
          complete {
            Json.obj(
              "color" -> JsNumber(WebServer.gameBoard.player4.color),
              "xStart" -> JsNumber(WebServer.gameBoard.player4.start._1),
              "yStart" -> JsNumber(WebServer.gameBoard.player4.start._2)
            ).toString()
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
              WebServer.gameBoard = WebServer.gameBoard.setBlockStoneOnField(WebServer.gameBoard.board((x, y)))
              JsBoolean(true).toString()
            }
          }
        } ~
        pathPrefix("removeStoneOnField") {
          path(IntNumber / IntNumber) { (x, y) =>
            complete {
              WebServer.gameBoard = WebServer.gameBoard.removeStoneOnField(WebServer.gameBoard.board((x, y)))
              JsBoolean(true).toString()
            }
          }
        } ~
        pathPrefix("resetPlayerStone") {
          path(IntNumber / IntNumber) { (x, y) =>
            complete {
              WebServer.gameBoard = WebServer.gameBoard.resetPlayerStone(WebServer.gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone])
              JsBoolean(true).toString()
            }
          }
        } ~
        pathPrefix("checkDestForPlayerStone") {
          path(IntNumber / IntNumber) { (x, y) =>
            complete {
              JsBoolean(WebServer.gameBoard.checkDestForPlayerStone(x, y)).toString()
            }
          }
        } ~
        pathPrefix("moveStone") {
          path(IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, xDest, yDest) =>
            complete {
              val start = WebServer.gameBoard.board((x, y))
              val dest = WebServer.gameBoard.board((xDest, yDest))
              val res = WebServer.gameBoard.moveStone(start, dest)
              val hitStone = res._1
              WebServer.gameBoard = res._2

              hitStone match {
                case Some(stone: PlayerStone) => Json.obj("sort" -> "p", "x" -> JsNumber(stone.x), "y" -> JsNumber(stone.y)).toString()
                case Some(_: BlockStone) => Json.obj("sort" -> "b").toString()
                case _ => Json.obj("sort" -> "f").toString()
              }
            }
          }
        } ~
        pathPrefix("forceMoveStone") {
          path(IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, xDest, yDest) =>
            complete {
              val start = WebServer.gameBoard.board((x, y))
              val dest = WebServer.gameBoard.board((xDest, yDest))
              WebServer.gameBoard = WebServer.gameBoard.forceMoveStone(start, dest)
              JsBoolean(true).toString()
            }
          }
        } ~
        pathPrefix("markPossibleMoves") {
          path(IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, playerColor, diced) =>
            complete {
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
        path("checkWin") {
          complete {
            JsBoolean(WebServer.gameBoard.checkWin).toString()
          }
        } ~
        pathPrefix("setField") {
          path(IntNumber / IntNumber / IntNumber / IntNumber / IntNumber) { (x, y, playerColor, stoneXstart, stoneYstart) =>
            complete {
              val stone = playerColor match {
                case 0 => BlockStone()
                case _ => PlayerStone(stoneXstart, stoneYstart, x, y, playerColor)
              }
              WebServer.gameBoard = WebServer.gameBoard.setField((x, y), Field(x, y, Some(stone)))
              JsBoolean(true).toString()
            }
          } ~
            path(IntNumber / IntNumber) { (x, y) =>
              complete {
                WebServer.gameBoard = WebServer.gameBoard.setField((x, y), Field(x, y, None))
                JsBoolean(true).toString()
              }
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
        }
    }
}
