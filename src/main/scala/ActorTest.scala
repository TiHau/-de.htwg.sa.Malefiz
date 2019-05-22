import akka.actor.{ Actor, ActorRef, ActorSystem, Props }

class MyActor(collector: ActorRef) extends Actor {
  override def receive: Receive = {
    case (from: Long, to: Long) =>
      var res: Long = 0
      for (i <- from to to) {
        res += i
      }
      collector ! res
      context.stop(self)
    case _ => println("invalid msg")
  }
}

class CollectActor extends Actor {
  var res: Long = 0

  override def receive: Receive = {
    case x: Long =>
      res += x
      println("collector received value " + x + "... New Res " + res)
    case _ => println("invalid msg")
  }
}

object Main extends App {
  val system = ActorSystem("mysystem")

  val n: Long = 200000000

  val collector = system.actorOf(Props[CollectActor], "collector")

  system.actorOf(Props(new MyActor(collector))) ! (1: Long, n / 4: Long)
  system.actorOf(Props(new MyActor(collector))) ! ((n / 4) + 1: Long, n / 2: Long)
  system.actorOf(Props(new MyActor(collector))) ! ((n / 2) + 1: Long, (n / 4) * 3: Long)
  system.actorOf(Props(new MyActor(collector))) ! ((n / 4) * 3 + 1: Long, n: Long)

  println("Fast sum: " + (n * (n + 1) / 2))

  Thread.sleep(4000)

  system.terminate()
}