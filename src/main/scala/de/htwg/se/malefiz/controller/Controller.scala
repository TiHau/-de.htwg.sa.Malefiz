package de.htwg.se.malefiz.controller

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse}
import akka.stream.ActorMaterializer
import com.google.inject.{Guice, Inject, Injector}
import com.typesafe.scalalogging.Logger
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.aview.ViewSocket
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import net.codingwell.scalaguice.InjectorExtensions._
import play.api.libs.json.{JsBoolean, JsObject, Json}

import scala.concurrent.duration.Duration
import scala.swing.Publisher
import scala.util.Success

case class Controller @Inject()() extends ControllerInterface with Publisher {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  private val six = 6
  private val logger = Logger(classOf[Controller])
  private val fileIO = injector.instance[FileIOInterface]
  private var chosenPlayerStone: Option[(Int, Int)] = None
  private var needToSetBlockStone = false
  private var activePlayerColor: Int = 3
  private var diced: Int = six
  private implicit val system = ActorSystem()
  private implicit val materializer = ActorMaterializer()
  private implicit val executionContext = system.dispatcher
  private var message = "Start a Game" // check win "Victory"


  override def loadSavedGame(): Unit = {
    fileIO.load(this)
    ViewSocket.updateGame()
  }

  override def saveGame(): Unit = {
    fileIO.save(this)
    ViewSocket.updateGame()
  }

  def newGame(playerCount: Int): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/new/" + playerCount)).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) =>
              val created = value.toBoolean
              if(created) {
                nextTurn()
              }
          }
        }
    }

  }

  def setPlayerCount(playerCount: Int): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/new/" + playerCount)).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          logger.info("Set player count successful")
        }
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
          }
        }
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
                if (stoneChoosen)
                  chosenPlayerStone = Some((x, y))
                  message = "Choose a Target Field"
            }
          }
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
                val sort: String = (hit \ "sort").get.toString.replace("\"", "")
                if (sort.equals("b")) {
                  needToSetBlockStone = true
                  message = "Set a BlockStone"
                } else if (sort.equals("p")) {
                  nextTurn()
                } else {
                  nextTurn()
                }
                chosenPlayerStone = None
            }
          }
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
            }
          }
      }
    }
    ViewSocket.updateGame()
  }

  def toJson: JsObject = {
    Json.obj("unimplemented" -> JsBoolean(true))
  }

}
