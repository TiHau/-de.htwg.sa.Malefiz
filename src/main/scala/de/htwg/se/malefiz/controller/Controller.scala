package de.htwg.se.malefiz.controller

import com.google.inject.name.Names
import com.google.inject.{Guice, Inject, Injector}
import com.typesafe.scalalogging.Logger
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.controller.State._
import de.htwg.se.malefiz.model.fileio.FileIOInterface
import de.htwg.se.malefiz.model.gameboard._
import de.htwg.se.malefiz.util.UndoManager
import net.codingwell.scalaguice.InjectorExtensions._

import scala.swing.Publisher

case class Controller @Inject() () extends ControllerInterface with Publisher {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  var gameBoard: GameBoardInterface = injector.instance[GameBoardInterface](Names.named("default")).createBoard
  activePlayer = gameBoard.player3
  private val six = 6
  private val logger = Logger(classOf[Controller])
  private val fileIO = injector.instance[FileIOInterface]
  private val undoManager = new UndoManager()
  private var chosenPlayerStone = gameBoard.player1.stones(0)
  private var destField = gameBoard.board((8, 0))
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
      case ChoosePlayerStone =>
        if (checkValidPlayerStone(x, y)) {
          chooseStone()
        }
      case ChooseTarget =>
        if (setTargetForPlayerStone(x, y)) {
          chooseTarget()
        }
      case SetBlockStone =>
        if (setTargetForBlockStone(x, y)) {
          setBlockStone()
        }
      case _ =>
    }
  }

  def reset(): Unit = {
    activePlayer = gameBoard.player3
    state = SetPlayerCount
    notifyObservers()
  }

  private def setBlockStone(): Unit = {
    undoManager.doStep(new BlockStoneCommand(destField, this))
    state = Print
    notifyObservers()
    state = BeforeEndOfTurn
    notifyObservers()
  }

  private def chooseStone(): Unit = {
    undoManager.doStep(new ChooseCommand(chosenPlayerStone, this))
    state = Print
    notifyObservers()
    state = ChooseTarget
    notifyObservers()
  }

  private def chooseTarget(): Unit = {
    undoManager.doStep(new MoveCommand(chosenPlayerStone, destField, this))
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

  private def dice(): Unit = diced = scala.util.Random.nextInt(six) + 1


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

  def setTargetForPlayerStone(x: Int, y: Int): Boolean = {
    if (gameBoard.checkDestForPlayerStone(x, y)) {
      destField = gameBoard.board((x, y))
      true
    } else {
      false
    }
  }

  private def setTargetForBlockStone(x: Int, y: Int): Boolean = {
    if (gameBoard.checkDestForBlockStone(x, y)) {
      destField = gameBoard.board((x, y))
      true
    } else {
      false
    }

  }

  private def checkValidPlayerStone(x: Int, y: Int): Boolean = {
    if (x >= 0 && x < 17 && y >= 0 && y < 16 && (gameBoard.board.contains((x, y))
      && gameBoard.board((x, y)).stone.isDefined && gameBoard.board((x, y)).stone.get.isInstanceOf[PlayerStone])) {
      var retBool: Boolean = false
      activePlayer.stones.filter(s=>s.x == gameBoard.board((x, y)).x && s.y == gameBoard.board((x, y)).y)
        .foreach(s=>{chosenPlayerStone = gameBoard.board((x, y)).stone.get.asInstanceOf[PlayerStone]
        retBool = true})
      retBool
    } else {
      false
    }
  }

  def setChoosenPlayerStone(newStone: PlayerStone): Unit = chosenPlayerStone = newStone

  def getChoosenPlayerStone: PlayerStone = chosenPlayerStone

  def setDestField(newField: Field): Unit = destField = newField

  def getDestField: Field = destField
}
