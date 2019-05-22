package de.htwg.se.malefiz.aview

import java.awt.{ Color, Font, Toolkit }

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ HttpMethods, HttpRequest, HttpResponse, StatusCodes }
import akka.http.scaladsl.model.ws.{ Message, TextMessage, WebSocketRequest }
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{ Flow, Keep, Sink, Source }
import akka.{ Done, NotUsed }
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.swing._
import scala.swing.event._
import scala.util.Success

class GUI extends Frame {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  import system.dispatcher

  // print each incoming strict text message
  val printSink: Sink[Message, Future[Done]] =
    Sink.foreach { _: Message =>
      update()
    }

  val helloSource: Source[Message, NotUsed] =
    Source.single(TextMessage("hello world!"))

  // the Future[Done] is the materialized value of Sink.foreach
  // and it is completed when the stream completes
  val flow: Flow[Message, Message, Future[Done]] =
    Flow.fromSinkAndSourceMat(printSink, helloSource)(Keep.left)

  // upgradeResponse is a Future[WebSocketUpgradeResponse] that
  // completes or fails when the connection succeeds or fails
  // and closed is a Future[Done] representing the stream completion from above
  val (upgradeResponse, closed) =
    Http().singleWebSocketRequest(WebSocketRequest("ws://localhost:8080/websocket"), flow)

  val connected = upgradeResponse.map { upgrade =>
    // just like a regular http request we can access response status which is available via upgrade.response.status
    // status code 101 (Switching Protocols) indicates that server support WebSockets
    if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
      Done
    } else {
      throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
    }
  }

  // in a real application you would not side effect here
  // and handle errors more carefully
  connected.onComplete(println)

  private val dim = Toolkit.getDefaultToolkit.getScreenSize
  private var message = "Ask Count First"
  private var activePlayer: Int = 3
  private var diced: Int = 0
  private var gbString: String = ""
  private var newG = false
  contents = new FlowPanel() {
    focusable = true
    listenTo(this.mouse.clicks)
    listenTo(this.keys)
    reactions += {
      case MouseClicked(_, point, _, _, _) =>
        Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8080/touch/" + (point.x - 20) / ((size.width - 50) / 17) + "/" + (point.y - 100) / ((size.height - 110) / 16))).onComplete {
          case Success(response: HttpResponse) =>
            update()
        }

    }

    override def paint(g: Graphics2D): Unit = {
      //Background
      background = Color.WHITE
      val activePlayerColorString: String = activePlayer match {
        case 1 => "Red"
        case 2 => "Green"
        case 3 => "Yellow"
        case _ => "Blue"
      }
      g.setFont(new Font("TimesRoman", Font.BOLD, size.width / 60))
      g.drawString("Player: " + activePlayerColorString, 40, 40)
      g.drawString("" + message, size.width / 3, 40)
      g.drawString("Diced: " + diced.toString, size.width - size.width / 6, 40)
      //Playground
      g.setColor(Color.LIGHT_GRAY)
      g.fillRect(10, 80, size.width - 20, size.height - 90)
      printingGameboard(g)
    }

    private def printingGameboard(g: Graphics2D): Unit = {
      var x: Int = 0
      var y: Int = 0
      val currentGB = gbString.replace(" ", "#").replace("###", "   ").trim
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
      contents += new MenuItem(Action("New") {
        message = "Start a Game"
        newG = true
        update()
      })
      contents += new MenuItem(Action("Save") {

        repaint()
      })
      contents += new MenuItem(Action("Load") {
        repaint()
      })
      contents += new MenuItem(Action("Quit") {
        sys.exit(0)
      })
    }
  }

  size = dim
  visible = true
  resizable = true
  title = "Malefitz"
  update()

  override def closeOperation(): Unit = sys.exit(0)

  def update(): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8080/toJson")).onComplete {
      case Success(response: HttpResponse) =>
        if (response.status.isSuccess()) {
          response.entity.toStrict(Duration(5000, "millis")).map {
            _.data
          }.map(_.utf8String).onComplete {
            case Success(value) =>
              val tmpJson = Json.parse(value)
              activePlayer = (tmpJson \ "activePlayer").get.toString.replace("\"", "").toInt
              diced = (tmpJson \ "diced").get.toString.replace("\"", "").toInt
              if (!newG) {
                message = (tmpJson \ "message").get.toString.replace("\"", "")
              }
              gbString = (tmpJson \ "gbstring").get.toString.replace("\"", "").replace("\\n", "\n").replace("§", "")
              println(gbString)
              message match {
                case "Victory" =>
                  val wonUI = new WinUI
                  wonUI.visible = true
                case "Start a Game" =>
                  newG = false
                  val countUI = new CountUI
                  countUI.visible = true
                case _ =>
              }
              repaint()
          }
        }
    }
  }

  def newGame(count: Int): Unit = {
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8080/new/" + count)).onComplete {
      case Success(response: HttpResponse) =>
    }
  }

  private class CountUI extends MainFrame {
    title = "Playercount"
    preferredSize = new Dimension(320, 70)
    location = new Point(dim.width / 3, dim.height / 3)
    contents = new FlowPanel() {
      contents += Button("2 Player") {
        newGame(2)
        update()
        dispose
      }
      contents += Button("3 Player") {
        newGame(3)
        update()
        dispose
      }
      contents += Button("4 Player") {
        newGame(4)
        update()
        dispose

      }
    }
  }

  private class WinUI extends MainFrame {
    val activePlayerColorString: String = activePlayer match {
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
        message = "Start a Game"
        newG = true
        update()
        dispose
      }
    }
  }

}

