package de.htwg.se.malefiz.rest

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.htwg.se.malefiz.Malefiz
import de.htwg.se.malefiz.util.JsonConverter


object Routes {

  val all: Route =
    get {
        path("") {
          complete {
            JsonConverter.gameToJson().toString()
          }
        } ~
        pathPrefix("touch") {
          path(IntNumber / IntNumber) { (column, row) =>
            complete {
              Malefiz.controller.takeInput(column, row)
              JsonConverter.gameToJson().toString()
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
              JsonConverter.gameToJson().toString()
            }
          }
        } ~
        path("turn") {
          complete {
            Malefiz.controller.endTurn()
            JsonConverter.gameToJson().toString()
          }
        } ~
        path("redo") {
          complete {
            Malefiz.controller.redo()
            JsonConverter.gameToJson().toString()
          }
        } ~
        path("undo") {
          complete {
            Malefiz.controller.undo()
            JsonConverter.gameToJson().toString()
          }
        }

    }

}
