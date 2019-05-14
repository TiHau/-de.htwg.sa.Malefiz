package de.htwg.se.malefiz.rest

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.htwg.se.malefiz.Malefiz


object Routes {

  val all: Route =
    get {
        path("") {
          complete {
            Malefiz.controller.toJson.toString
          }
        } ~
        pathPrefix("touch") {
          path(IntNumber / IntNumber) { (column, row) =>
            complete {
              Malefiz.controller.takeInput(column, row)
              Malefiz.controller.toJson.toString
            }
          }
        } ~
        pathPrefix("new") {
          path(IntNumber) { count =>
            complete {
              Malefiz.controller.newGame(if (count > 4) {
                4
              } else if (count < 2) {
                2
              } else {
                count
              }
              )
              Malefiz.controller.toJson.toString
            }
          }
        } ~
        path("turn") {
          complete {
            Malefiz.controller.endTurn()
            Malefiz.controller.toJson.toString
          }
        } ~
        path("redo") {
          complete {
            Malefiz.controller.redo()
            Malefiz.controller.toJson.toString
          }
        } ~
        path("undo") {
          complete {
            Malefiz.controller.undo()
            Malefiz.controller.toJson.toString
          }
        }

    }

}
