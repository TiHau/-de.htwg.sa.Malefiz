package de.htwg.se.malefiz.rest

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.htwg.se.malefiz.aview.ViewSocket


object Routes {

  val all: Route =
    get {
        path("") {
          complete {
            WebServer.controller.toJson.toString
          }
        } ~
        pathPrefix("touch") {
          path(IntNumber / IntNumber) { (column, row) =>
            complete {
              WebServer.controller.takeInput(column, row)
              WebServer.controller.toJson.toString
            }
          }
        } ~
        pathPrefix("new") {
          path(IntNumber) { count =>
            complete {
              WebServer.controller.newGame(if (count > 4) {
                4
              } else if (count < 2) {
                2
              } else {
                count
              }
              )
              WebServer.controller.toJson.toString
            }
          }
        } ~
        path("save") {
          complete {
            WebServer.controller.saveGame()
            WebServer.controller.toJson.toString
          }
        } ~
        path("load") {
          complete {
            WebServer.controller.loadSavedGame()
            WebServer.controller.toJson.toString
          }
        } ~
          path("toJson") {
            complete {
              WebServer.controller.toJson.toString
            }
          } ~
      path("websocket") {
        handleWebSocketMessages(ViewSocket.listen())
      }


    }

}
