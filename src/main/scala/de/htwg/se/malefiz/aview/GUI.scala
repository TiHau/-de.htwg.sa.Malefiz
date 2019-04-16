package de.htwg.se.malefiz.aview

import java.awt.{ Color, Font, Toolkit }

import de.htwg.se.malefiz.util.Observer
import de.htwg.se.malefiz.controller.{ ControllerInterface, State }

import scala.swing.event._
import scala.swing._
import de.htwg.se.malefiz.controller.State._

class GUI(controller: ControllerInterface) extends Frame with Observer {
  private val dim = Toolkit.getDefaultToolkit.getScreenSize
  private var message = "Ask Count First"
  controller.add(this)
  contents = new FlowPanel() {
    focusable = true
    listenTo(this.mouse.clicks)
    listenTo(this.keys)
    reactions += {
      case MouseClicked(_, point, _, _, _) => controller.takeInput((point.x - 20) / ((size.width - 50) / 17), (point.y - 100) / ((size.height - 110) / 16))
      case KeyPressed(_, Key.Enter, _, _) => controller.endTurn()
      case KeyPressed(_, Key.BackSpace, _, _) => controller.undo()
    }
    override def paint(g: Graphics2D): Unit = {
      //Background
      background = Color.WHITE
      val activePlayerColorString: String = controller.activePlayer.color match {
        case 1 => "Red"
        case 2 => "Green"
        case 3 => "Yellow"
        case _ => "Blue"
      }
      g.setFont(new Font("TimesRoman", Font.BOLD, size.width / 60))
      g.drawString("Player: " + activePlayerColorString, 40, 40)
      g.drawString("" + message, size.width / 3, 40)
      g.drawString("Diced: " + controller.diced.toString, size.width - size.width / 6, 40)
      //Playground
      g.setColor(Color.LIGHT_GRAY)
      g.fillRect(10, 80, size.width - 20, size.height - 90)
      printingGameboard(g)
    }

    private def printingGameboard(g: Graphics2D): Unit = {
      var x: Int = 0
      var y: Int = 0
      val currentGB = controller.gameBoard.toString.replace(" ", "#").replace("###", "   ").trim
      var check = 0
      var count = 0
      currentGB.foreach {
        case '|' =>
          check += 1
          if (check == 3) {
            drawOvalSmall()
            check = 0
            x += 1
          } else if (check == 1) {
            drawRect(new Color(244, 164, 96))
            drawOvalNormal(Color.BLACK)
          }
        case '\n' =>
          y += 1
          x = 0
          check = 0
        case ' ' =>
          check += 1
          if (check == 3) {
            drawRect(new Color(244, 164, 96))
            check = 0
            x += 1
          }
        case '-' => setStoneColorWithoutBackground(Color.WHITE)
        case 'o' => setStoneColorWithoutBackground(Color.BLACK)
        case '1' => setStoneColorWithoutBackground(Color.RED)
        case '2' => setStoneColorWithoutBackground(Color.GREEN)
        case '3' => setStoneColorWithoutBackground(Color.YELLOW)
        case '4' => setStoneColorWithoutBackground(Color.BLUE)
        case 'x' => setStoneColorWithBackgroundPainting(new Color(238, 118, 0))
        case 'G' => setStoneColorWithAlternateBackgroundPainting(Color.RED)
        case 'K' => setStoneColorWithAlternateBackgroundPainting(Color.BLUE)
        case 'J' => setStoneColorWithAlternateBackgroundPainting(Color.YELLOW)
        case 'H' => setStoneColorWithAlternateBackgroundPainting(Color.GREEN)
        case 'B' => setStoneColorWithBackgroundPainting(Color.WHITE)
        case _ =>
      }

      def setStoneColorWithoutBackground(color: Color): Unit = {
        if (check == 1) {
          check += 1
          g.setColor(color)
        }
      }

      def setStoneColorWithBackgroundPainting(color: Color): Unit = {
        if (check == 1) {
          drawOvalNormal(new Color(238, 118, 0))
          check += 1
          g.setColor(color)
        }
      }
      def setStoneColorWithAlternateBackgroundPainting(color: Color): Unit = {
        if (check == 1) {
          drawOvalNormal(Color.MAGENTA)
          check += 1
          g.setColor(color)
        }
      }

      def drawOvalSmall(): Unit = {
        g.fillOval(20 + ((size.width - 50) / 17) * x, 100 + ((size.height - 110) / 16) * y,
          ((size.width - 50) / 17) - 6, ((size.height - 110) / 16) - 6)
      }

      def drawOvalNormal(color: Color): Unit = {
        g.setColor(color)
        g.fillOval(20 + ((size.width - 50) / 17) * x, 100 + ((size.height - 110) / 16) * y,
          ((size.width - 50) / 17) - 2, ((size.height - 110) / 16) - 2)
      }

      def drawRect(color: Color): Unit = {
        if (count < 272) {
          g.setColor(color)
          g.fillRect(20 + ((size.width - 50) / 17) * x, 100 + ((size.height - 110) / 16) * y,
            (size.width - 50) / 17, (size.height - 110) / 16)
          count += 1
        }
      }
    }
  }

  menuBar = new MenuBar {
    contents += new Menu("File") {
      mnemonic = Key.F
      contents += new MenuItem(Action("New") {controller.reset()})
      contents += new MenuItem(Action("Save") {
        controller.saveGame()
        repaint()
      })
      contents += new MenuItem(Action("Load") {
        controller.loadSavedGame()
        repaint()
      })
      contents += new MenuItem(Action("Quit") {sys.exit(0)})
    }
    contents += new Menu("Edit") {
      mnemonic = Key.E
      contents += new MenuItem(Action("Undo") {
        controller.undo()
        repaint()
      })
      contents += new MenuItem(Action("Redo") {
        controller.redo()
        repaint()
      })
    }
  }

  size = dim
  visible = true
  resizable = true
  title = "Malefitz"
  controller.reset()

  override def closeOperation(): Unit = sys.exit(0)
  override def update(): Unit = {
    message = controller.getState match {
      case Print | EndTurn => message
      case SetBlockStone => "Set a BlockStone"
      case ChoosePlayerStone => "Chose one of your Stones"
      case ChooseTarget => "Chose a Target Field"
      case PlayerWon =>
        val wonUI = new WinUI
        wonUI.visible = true
        message
      case SetPlayerCount =>
        val countUI = new CountUI
        countUI.visible = true
        message
      case BeforeEndOfTurn => "Press Enter to end your turn or Backspace to undo"
    }
    repaint()
  }
  private class CountUI extends MainFrame {
    title = "Playercount"
    preferredSize = new Dimension(320, 70)
    location = new Point(dim.width / 3, dim.height / 3)
    contents = new FlowPanel() {
      contents += Button("2 Player") {
        controller.newGame(2)
        dispose
      }
      contents += Button("3 Player") {
        controller.newGame(3)
        dispose
      }
      contents += Button("4 Player") {
        controller.newGame(4)
        dispose
      }
    }
  }
  private class WinUI extends MainFrame {
    val activePlayerColorString: String = controller.activePlayer.color match {
      case 1 => "Red"
      case 2 => "Green"
      case 3 => "Yellow"
      case _ => "Blue"
    }
    title = "Victory"
    preferredSize = new Dimension(400, 120)
    location = new Point(dim.width / 3, dim.height / 3)
    contents = new FlowPanel() {
      contents += new Label("Player " + activePlayerColorString + " Won the Game!")
      contents += Button("Exit") {
        sys.exit(0)
      }
      contents += Button("New Game") {
        controller.reset()
        dispose()
      }
    }
  }
}

