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
  private var chosenPlayerStone: PlayerStone = _
  private var destField: Field = _
  private var state: State.Value = Print

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
    state = if (!gameBoard.checkWin) {
      undoManager.clear()
      activePlayer = if (activePlayer.color == 1) {
        gameBoard.player4
      } else if (activePlayer.color == 4 && gameBoard.playerCount >= 3) {
        gameBoard.player2
      } else if (activePlayer.color == 2 && gameBoard.playerCount == 4) {
        gameBoard.player3
      } else {
        gameBoard.player1
      }
      diced = scala.util.Random.nextInt(six) + 1
      state = Print
      notifyObservers() //print GameBoard
      needToSetBlockStone = false
      ChoosePlayerStone
    } else {
      PlayerWon
    }
    notifyObservers()
  }

  def takeInput(x: Int, y: Int): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    state match {
      case ChoosePlayerStone =>
        val responseFuture: Future[HttpResponse] = Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8081/isOneOfMyStonesThere/" + x + "/" + y + "/" + activePlayer.color))

        responseFuture.onComplete {
          case Success(response: HttpResponse) => println("It works!\n"+response.entity)
          case Failure(_) => println("failed!")
        }

        Await.result(responseFuture, Duration(5000, "millis"))


        if (gameBoard.board.contains((x, y))
          && gameBoard.board((x, y)).stone.isDefined
          && gameBoard.board((x, y)).stone.get.isInstanceOf[PlayerStone]
          && activePlayer.color == gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone].playerColor) {
          chosenPlayerStone = gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone]
          undoManager.doStep(new ChooseCommand(chosenPlayerStone, this))
          state = Print
          notifyObservers()
          state = ChooseTarget
        }
      case ChooseTarget =>
        if (gameBoard.checkDestForPlayerStone(x, y)) {
          destField = gameBoard.board((x, y))
          undoManager.doStep(new MoveCommand(chosenPlayerStone, destField, this))
          state = Print
          notifyObservers()
          state = if (needToSetBlockStone) {
            SetBlockStone
          } else {
            BeforeEndOfTurn
          }
        }
      case SetBlockStone =>
        if (gameBoard.checkDestForBlockStone(x, y)) {
          destField = gameBoard.board((x, y))
          undoManager.doStep(new BlockStoneCommand(destField, this))
          state = Print
          notifyObservers()
          state = BeforeEndOfTurn
        }
      case _ =>
    }
    notifyObservers()
  }

  def reset(): Unit = {
    activePlayer = gameBoard.player3
    state = SetPlayerCount
    notifyObservers()
  }

  def setChoosenPlayerStone(newStone: PlayerStone): Unit = chosenPlayerStone = newStone

  def getChoosenPlayerStone: PlayerStone = chosenPlayerStone

  def setDestField(newField: Field): Unit = destField = newField

  def getDestField: Field = destField

  override def setGameBoad(newGameBoard: GameBoardInterface): Unit = gameBoard = newGameBoard

  def toJson: JsObject = {
    Json.obj("unimplemented" -> JsBoolean(true))
  }

}
