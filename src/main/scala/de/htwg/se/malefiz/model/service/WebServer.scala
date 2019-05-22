package de.htwg.se.malefiz.model.service

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.google.inject.name.Names
import com.google.inject.{ Guice, Injector }
import de.htwg.se.malefiz.MalefizModule
import de.htwg.se.malefiz.model.gameboard.GameBoardInterface
import net.codingwell.scalaguice.InjectorExtensions._

import scala.io.StdIn

object WebServer {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  var gameBoard: GameBoardInterface = injector.instance[GameBoardInterface](Names.named("default")).createBoard

  def main(args: Array[String]) {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val bindingFuture = Http().bindAndHandle(Routes.all, "0.0.0.0", 8081)
    println(s"Server online at http://0.0.0.0:8081/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

