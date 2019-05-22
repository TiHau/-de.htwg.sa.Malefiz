package de.htwg.se.malefiz.aview

import akka.NotUsed
import akka.http.scaladsl.model.ws.{ Message, TextMessage }
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }
import de.htwg.se.malefiz.rest.WebServer

object ViewSocket {

  private var browserConnections: List[TextMessage => Unit] = List()

  def listen(): Flow[Message, Message, NotUsed] = {
    val inbound: Sink[Message, Any] = Sink.foreach(_ => ())
    val outbound: Source[Message, SourceQueueWithComplete[Message]] = Source.queue[Message](Int.MaxValue, OverflowStrategy.fail)
    Flow.fromSinkAndSourceMat(inbound, outbound)((_, outboundMat) => {
      browserConnections ::= outboundMat.offer
      NotUsed
    })
  }

  def updateGame(): Unit = {
    for (connection <- browserConnections) connection(TextMessage.Strict(WebServer.controller.toJson.toString()))
  }

}
