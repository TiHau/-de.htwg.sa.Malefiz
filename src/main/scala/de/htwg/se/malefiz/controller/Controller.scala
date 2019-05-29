package de.htwg.se.malefiz.controller

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse}
import akka.stream.ActorMaterializer
import com.google.inject.{Guice, Inject, Injector}
import com.typesafe.scalalogging.Logger
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.aview.ViewSocket
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString, JsValue, Json}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.swing.Publisher
import scala.util.{Failure, Success}

case class Controller @Inject()() extends ControllerInterface with Publisher {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  private val six = 6
  private val logger = Logger(classOf[Controller])
  private var chosenPlayerStone: Option[(Int, Int)] = None
  private var needToSetBlockStone = false
  private var needToMove = true
  private var activePlayerColor: Int = 3
  private var diced: Int = six
  private implicit val system = ActorSystem()
  private implicit val materializer = ActorMaterializer()
  private implicit val executionContext = system.dispatcher
  private var message = "Start a Game" // check win "Victory"


  def newGame(playerCount: Int): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/new/" + playerCount)).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) =>
              val created = value.toBoolean
              if (created) {
                ViewSocket.updateGame()
                nextTurn()
              }
            case Failure(_) =>
          }
        }
      case Failure(_) =>
    }

  }

  def setPlayerCount(playerCount: Int): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/new/" + playerCount)).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          logger.info("Set player count successful")
        }
      case Failure(_) =>
    }
  }


  private def nextTurn(): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/playerCount")).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) =>
              val playerCount = value.toInt
              activePlayerColor = if (activePlayerColor == 1) {
                4
              } else if (activePlayerColor == 4 && playerCount >= 3) {
                2
              } else if (activePlayerColor == 2 && playerCount == 4) {
                3
              } else {
                1
              }
              diced = scala.util.Random.nextInt(six) + 1
              needToSetBlockStone = false
              chosenPlayerStone = None
              message = "Choose one of your Stones"
              ViewSocket.updateGame()
            case Failure(_) =>
          }
        }
      case Failure(_) =>
    }
  }

  def takeInput(x: Int, y: Int): Unit = {
    //check if player clicked on one of his stones
    if (!needToSetBlockStone) {
      Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/markPossibleMoves/" + x + "/" + y + "/" + activePlayerColor + "/" + diced)).onComplete {
        case Success(response: HttpResponse) =>
          if (response.status.isSuccess()) {
            response.entity.toStrict(Duration(5000, "millis")).map {
              _.data
            }.map(_.utf8String).onComplete {
              case Success(value) =>
                val stoneChoosen = value.toBoolean
                if (stoneChoosen) {
                  chosenPlayerStone = Some((x, y))
                  needToMove = true
                  message = "Choose a Target Field"
                }
              case Failure(_) =>
            }
          }
        case Failure(_) =>
      }
    }
    //check if player clicked on a field that he can move to
    if (chosenPlayerStone.isDefined) {
      Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/moveStone/" + chosenPlayerStone.get._1 + "/" + chosenPlayerStone.get._2 + "/" + x + "/" + y)).onComplete {
        case Success(response: HttpResponse) =>
          if (response.status.isSuccess()) {
            response.entity.toStrict(Duration(5000, "millis")).map {
              _.data
            }.map(_.utf8String).onComplete {
              case Success(value) =>
                val hit = Json.parse(value)
                val moved: Boolean = (hit \ "moved").get.toString.replace("\"", "").toBoolean
                if (moved) {
                  println("here")
                  needToMove = false
                }

                val sort: String = (hit \ "sort").get.toString.replace("\"", "")
                sort match {
                  case "b" =>
                    needToSetBlockStone = true
                    message = "Set a BlockStone"
                  case "p" =>
                    val xN: Int = (hit \ "x").get.toString.replace("\"", "").toInt
                    val yN: Int = (hit \ "y").get.toString.replace("\"", "").toInt
                    val startX: Int = (hit \ "startX").get.toString.replace("\"", "").toInt
                    val startY: Int = (hit \ "startY").get.toString.replace("\"", "").toInt
                    val color: Int = (hit \ "color").get.toString.replace("\"", "").toInt
                    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/resetPlayerStone/" + xN + "/" + yN + "/" + startX + "/" + startY + "/" + color)).onComplete {
                      case Success(response: HttpResponse) =>
                        if (response.status.isSuccess()) {
                          response.entity.toStrict(Duration(5000, "millis")).map {
                            _.data
                          }.map(_.utf8String).onComplete {
                            case Success(value) =>
                              val playerStoneReset = value.toBoolean
                              if (playerStoneReset) {
                                if (!needToSetBlockStone && !needToMove) {
                                  nextTurn()
                                }
                              }
                            case Failure(_) =>
                          }
                        }
                      case Failure(_) =>
                    }

                  case _ =>
                    if (!needToSetBlockStone && !needToMove)
                      nextTurn()
                }
                chosenPlayerStone = None
              case Failure(_) =>
            }
          }
        case Failure(_) =>
      }
    }
    //setBlockStone if needed
    if (needToSetBlockStone) {
      Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/setBlockStoneOnField/" + x + "/" + y)).onComplete {
        case Success(response: HttpResponse) =>
          if (response.status.isSuccess()) {
            response.entity.toStrict(Duration(5000, "millis")).map {
              _.data
            }.map(_.utf8String).onComplete {
              case Success(value) =>
                val blockStoneSet = value.toBoolean
                if (blockStoneSet)
                  nextTurn()
              case Failure(_) =>
            }
          }
        case Failure(_) =>
      }
    }
    ViewSocket.updateGame()
  }

  def toJson: JsObject = {
    var gameBoardAsJson: JsValue = Json.obj("failed" -> JsBoolean(true))
    var gameBoardAsString: String = ""
    val resp: Future[HttpResponse] = Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/getJson"))
    resp.onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) =>
              gameBoardAsJson = Json.parse(value)
            case Failure(_) =>
          }
        }
      case Failure(_) =>
    }
    Await.result(resp, Duration(8000, "millis"))
    val resp2: Future[HttpResponse] = Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/getString"))
    resp2.onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) => println(value)
              gameBoardAsString = value + "§"
            case Failure(_) =>
          }
        }
      case Failure(_) =>
    }
    Await.result(resp2, Duration(5000, "millis"))
    while (!gameBoardAsString.endsWith("§")) {
      Thread.sleep(100)
    }
    Json.obj(
      "activePlayer" -> JsNumber(activePlayerColor),
      "diced" -> JsString(diced.toString),
      "message" -> JsString(message),
      "rows" -> gameBoardAsJson,
      "gbstring" -> gameBoardAsString
    )

  }

  override def loadSavedGame(): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/load")).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) =>
              val js = Json.parse(value)
              activePlayerColor = (js \ "activePlayer").get.toString().toInt
              diced = (js \ "diced").get.toString().toInt
              ViewSocket.updateGame()
            case Failure(_) =>
          }
        }
      case Failure(_) =>
    }

    ViewSocket.updateGame()
  }

  override def saveGame(): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/save/" + activePlayerColor + "/" + diced)).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {

        }
      case Failure(_) =>
    }
  }

}
