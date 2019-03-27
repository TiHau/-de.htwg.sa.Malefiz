package de.htwg.se.malefiz.controller

import com.google.inject.name.Names
import com.google.inject.{Guice, Inject, Injector}
import net.codingwell.scalaguice.InjectorExtensions._
import com.typesafe.scalalogging.Logger
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.util.UndoManager
import de.htwg.se.malefiz.model.gameboard._
import de.htwg.se.malefiz.controller.State._
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import play.api.libs.json.JsObject

import scala.swing.Publisher

case class Controller @Inject()() extends ControllerInterface with Publisher {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  var gameBoard: GameBoardInterface = injector.instance[GameBoardInterface](Names.named("default")).createBoard
  activePlayer = gameBoard.player3
  private val six = 6
  private val logger = Logger(classOf[Controller])
  private val fileIO = injector.instance[FileIOInterface]
  private val undoManager = new UndoManager()
  private var chosenPlayerStone = gameBoard.player1.stones(0)

  override def loadSavedGame(): Unit = fileIO.load(this)

  override def saveGame(): Unit = fileIO.save(this)

  def newGame(playerCount: Int): Unit = {
    if (playerCount <= 2) {
      gameBoard = injector.instance[GameBoardInterface](Names.named("tiny")).createBoard
    } else if (playerCount == 3) {
      gameBoard = injector.instance[GameBoardInterface](Names.named("small")).createBoard
    } else {
      gameBoard = injector.instance[GameBoardInterface](Names.named("default")).createBoard
    }
    nextTurn()
  }

  def setPlayerCount(playerCount: Int): Unit = {
    if (playerCount <= 2) {
      gameBoard = injector.instance[GameBoardInterface](Names.named("tiny")).createBoard
    } else if (playerCount == 3) {
      gameBoard = injector.instance[GameBoardInterface](Names.named("small")).createBoard
    } else {
      gameBoard = injector.instance[GameBoardInterface](Names.named("default")).createBoard
    }
  }

  def undo(): Unit = {
    logger.info("Active Player pressed undo")
    undoManager.undoStep()
    val oldState = state
    state = Print
    notifyObservers()
    oldState match {
      case ChooseTarget =>
        state = ChoosePlayerStone
        notifyObservers()
      case BeforeEndOfTurn =>
        if (needToSetBlockStone) {
          state = SetBlockStone
        } else {
          state = ChooseTarget
        }
        notifyObservers()
      case SetBlockStone =>
        state = ChooseTarget
        notifyObservers()
      case ChoosePlayerStone =>
        state = ChoosePlayerStone
        notifyObservers()
    }
  }

  def redo(): Unit = {
    if (!undoManager.isRedoStackEmpty()) {
      logger.info("Active Player pressed redo")
      undoManager.redoStep()
      val oldState = state
      state = Print
      notifyObservers()
      oldState match {
        case ChoosePlayerStone =>
          state = ChooseTarget
          notifyObservers()
        case ChooseTarget =>
          if (needToSetBlockStone) {
            state = SetBlockStone
          } else {
            state = BeforeEndOfTurn
          }
          notifyObservers()
        case SetBlockStone =>
          state = BeforeEndOfTurn
          notifyObservers()
        case BeforeEndOfTurn =>
      }
    }
  }

  def endTurn(): Unit = {
    if (state == BeforeEndOfTurn) {
      state = EndTurn
      nextTurn()
    }
  }

  private def nextTurn(): Unit = {
    if (!gameBoard.checkWin) {
      undoManager.clear()
      changePlayer()
      dice()
      state = Print
      notifyObservers() //print GameBoard
      state = ChoosePlayerStone
      needToSetBlockStone = false
      notifyObservers()
    } else {
      state = PlayerWon
      notifyObservers()
    }
  }

  def takeInput(x: Int, y: Int): Unit = {
    state match {
      case ChoosePlayerStone => chosePlayerStone(x, y)
      case ChooseTarget => setTargetField(x, y)
      case SetBlockStone => setBlockStone(x, y)
      case _ =>
    }
  }

  def reset(): Unit = {
    activePlayer = gameBoard.player3
    state = SetPlayerCount
    notifyObservers()
  }

  private def dice(): Unit = {
    diced = scala.util.Random.nextInt(six) + 1
  }

  private def changePlayer(): Unit = {
    if (activePlayer.color == 1) {
      activePlayer = gameBoard.player4
    } else if (activePlayer.color == 4 && gameBoard.playerCount >= 3) {
      activePlayer = gameBoard.player2
    } else if (activePlayer.color == 2 && gameBoard.playerCount == 4) {
      activePlayer = gameBoard.player3
    } else if (activePlayer.color == 3) {
      activePlayer = gameBoard.player1
    } else {
      activePlayer = gameBoard.player1
    }
  }

  private def setTargetField(x: Int, y: Int): Unit = {
    if (gameBoard.checkDestForPlayerStone(x, y)) {
      undoManager.doStep(new MoveCommand(chosenPlayerStone, gameBoard.board(x)(y).asInstanceOf[Field], this))
      state = Print
      notifyObservers()
      if (needToSetBlockStone) {
        state = SetBlockStone
        notifyObservers()
      } else {
        state = BeforeEndOfTurn
        notifyObservers()
      }
    }
  }

  private def setBlockStone(x: Int, y: Int): Unit = {
    if (gameBoard.checkDestForBlockStone(x, y)) {
      undoManager.doStep(new BlockStoneCommand(gameBoard.board(x)(y).asInstanceOf[Field], this))
      state = Print
      notifyObservers()
      state = BeforeEndOfTurn
      notifyObservers()
    }
  }

  private def chosePlayerStone(x: Int, y: Int): Unit = {
    if (x >= 0 && x < 17 && y >= 0 && y < 16 && (!gameBoard.board(x)(y).isFreeSpace() && gameBoard.board(x)(y).asInstanceOf[Field].stone.sort == 'p')) {
      for (s <- activePlayer.stones) {
        if ((s.actualField.asInstanceOf[Field].x == gameBoard.board(x)(y).asInstanceOf[Field].x)
          && (s.actualField.asInstanceOf[Field].y == gameBoard.board(x)(y).asInstanceOf[Field].y)) {

          undoManager.doStep(new ChooseCommand(gameBoard.board(x)(y).asInstanceOf[Field].stone.asInstanceOf[PlayerStone], this))
          state = Print
          notifyObservers()
          state = ChooseTarget
          notifyObservers()

        }
      }
    }
  }

  def setChosenPlayerStone(newStone: PlayerStone): Unit = chosenPlayerStone = newStone

  def getChosenPlayerStone: PlayerStone = chosenPlayerStone
}
