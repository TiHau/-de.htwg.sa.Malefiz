package de.htwg.se.malefiz.aview

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.{Done, NotUsed}
import com.typesafe.scalalogging.Logger
import de.htwg.se.malefiz.util.Observer
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Success

case class TUI() extends Observer {
  private val logger = Logger(classOf[TUI])
  var checkFirst: Boolean = true
  logger.info("TUI Malefiz\n")
  logger.info("Welcome!!!\n")
  private var message = "Ask Count First"
  private var activePlayer: Int = 3
  private var diced: Int = 0
  private var gbString: String = ""

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

  //  controller.add(this)

  def printGameBoard(): Unit = {
    logger.info("\nGameboard: \n" +
      gbString +
      "\nPlayer: " + activePlayer +
      "\nDiced: " + diced
      + "\n")
  }

  def readLine(): Unit = {
    val x = readInput match {
      case Some(i) => i
      case None => 0
    }
    val y = readInput match {
      case Some(i) => i
      case None => 0
    }
    Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8080/touch/" + x + "/" + y)).onComplete {
      case Success(response: HttpResponse) =>
        update()
    }
  }

  private def readInput: Option[Int] = {
    val line = scala.io.StdIn.readLine()
    line match {
      case "count" =>
        Http().singleRequest(HttpRequest(HttpMethods.GET, "http://localhost:8080/new/" + 4)).onComplete {
          case Success(response: HttpResponse) =>
            update()
        }
        None
      case "exit" => sys.exit(0)
      case "restart" =>
        checkFirst = true
        //   controller.reset()
        None
      case _ =>
        try {
          Some(line.toInt)
        } catch {
          case _: Throwable => None
        }
    }
  }

  override def update(): Unit = {
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
              message = (tmpJson \ "message").get.toString.replace("\"", "")

              gbString = (tmpJson \ "gbstring").get.toString.replace("\"", "").replace("\\n", "\n")
              println(gbString)
              message match {
                case "Victory" =>
                  logger.info("Player: " + activePlayer + " Won the Game\n")
                case _ => logger.info(message + "\n")
              }
              printGameBoard()
          }
        }
    }

  }
}
