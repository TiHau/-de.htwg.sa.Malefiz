package de.htwg.se.malefiz.controller

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethod, HttpMethods, HttpRequest, HttpResponse}
import com.google.inject.name.Names
import com.google.inject.{Guice, Inject, Injector}
import com.typesafe.scalalogging.Logger
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.controller.State._
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import de.htwg.se.malefiz.model.gameboard._
import de.htwg.se.malefiz.util.UndoManager
import net.codingwell.scalaguice.InjectorExtensions._
import play.api.libs.json.{JsBoolean, JsObject, Json}
import akka.actor.ActorSystem

import scala.util.{Failure, Success}
import akka.http.scaladsl.Http
import akka.util.ByteString
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.swing.Publisher

case class Controller @Inject()() extends ControllerInterface with Publisher {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  var gameBoard: GameBoardInterface = injector.instance[GameBoardInterface](Names.named("default")).createBoard
  activePlayer = gameBoard.player3
  private val six = 6
  private val logger = Logger(classOf[Controller])
  private val fileIO = injector.instance[FileIOInterface]
  private val undoManager = new UndoManager()
  private var chosenPlayerStone: Option[(Int, Int)] = None
  private var needToSetBlockStone = false
  private var destField: Field = _
  private var state: State.Value = Print
  private var activePlayerColor: Int = _

  override def getState: State.Value = state

  override def setState(newState: State.Value): Unit = state = newState

  override def loadSavedGame(): Unit = {
    state = ChoosePlayerStone
    undoManager.clear()
    fileIO.load(this)
    notifyObservers()
  }

  override def saveGame(): Unit = {
    while (!undoManager.isUndoStackEmpty)
      undo()
    undoManager.clear()
    fileIO.save(this)
    notifyObservers()
  }

  def newGame(playerCount: Int): Unit = {
    gameBoard = playerCount match {
      case 2 => injector.instance[GameBoardInterface](Names.named("tiny")).createBoard
      case 3 => injector.instance[GameBoardInterface](Names.named("small")).createBoard
      case _ => injector.instance[GameBoardInterface](Names.named("default")).createBoard
    }
    nextTurn()
  }

  def setPlayerCount(playerCount: Int): Unit = {
    gameBoard = playerCount match {
      case 2 => injector.instance[GameBoardInterface](Names.named("tiny")).createBoard
      case 3 => injector.instance[GameBoardInterface](Names.named("small")).createBoard
      case _ => injector.instance[GameBoardInterface](Names.named("default")).createBoard
    }
  }

  def undo(): Unit = {
    logger.info("Active Player pressed undo")
    undoManager.undoStep()
    val oldState = state
    state = Print
    notifyObservers()
    state = oldState match {
      case ChooseTarget | ChoosePlayerStone => ChoosePlayerStone
      case BeforeEndOfTurn =>
        if (needToSetBlockStone) {
          SetBlockStone
        } else {
          ChooseTarget
        }
      case SetBlockStone => ChooseTarget
    }
    notifyObservers()
  }

  def redo(): Unit = {
    if (!undoManager.isRedoStackEmpty) {
      logger.info("Active Player pressed redo")
      undoManager.redoStep()
      val oldState = state
      state = Print
      notifyObservers()
      state = oldState match {
        case ChoosePlayerStone => ChooseTarget
        case ChooseTarget =>
          if (needToSetBlockStone) {
            SetBlockStone
          } else {
            BeforeEndOfTurn
          }
        case SetBlockStone => BeforeEndOfTurn
        case _ => state
      }
      notifyObservers()
    }
  }

  def endTurn(): Unit = {
    if (state == BeforeEndOfTurn) {
      state = EndTurn
      nextTurn()
    }
  }

  private def nextTurn(): Unit = {
    activePlayerColor = if (activePlayerColor == 1) {
      4
    } else if (activePlayerColor == 4 && gameBoard.playerCount >= 3) {
      2
    } else if (activePlayerColor == 2 && gameBoard.playerCount == 4) {
      3
    } else {
      1
    }
    diced = scala.util.Random.nextInt(six) + 1
    needToSetBlockStone = false
    chosenPlayerStone = None
    notifyObservers()
  }

  def takeInput(x: Int, y: Int): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    //check if player clicked on one of his stones
    if (!needToSetBlockStone) {
      Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/markPossibleMoves/" + x + "/" + y + "/" + activePlayer.color + "/" + diced)).onComplete {
        case Success(response: HttpResponse) =>
          if (response.status.isSuccess()) {
            response.entity.toStrict(Duration(5000, "millis")).map {
              _.data
            }.map(_.utf8String).onComplete {
              case Success(value) =>
                val stoneChoosen = value.toBoolean
                if (stoneChoosen)
                  chosenPlayerStone = Some((x, y))
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

    notifyObservers()
  }

  def reset(): Unit = {
    activePlayer = gameBoard.player3
    state = SetPlayerCount
    notifyObservers()
  }

  def setDestField(newField: Field): Unit = destField = newField

  def getDestField: Field = destField

  override def setGameBoad(newGameBoard: GameBoardInterface): Unit = gameBoard = newGameBoard

  def toJson: JsObject = {
    Json.obj("unimplemented" -> JsBoolean(true))
  }

}
