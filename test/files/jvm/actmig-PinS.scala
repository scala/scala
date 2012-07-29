import scala.actors._
import scala.concurrent.util.duration._
import scala.concurrent.{ Promise, Await }

import scala.actors.Actor._

/* PinS, Listing 32.1: A simple actor
 */
object SillyActor extends Actor {
  def act() {
    for (i <- 1 to 5)
      println("I'm acting!")

    println("Post stop")
  }
}

object SeriousActor extends Actor {
  def act() {
    for (i <- 1 to 5)
      println("To be or not to be.")
  }
}

/* PinS, Listing 32.3: An actor that calls react
 */
object NameResolver extends Actor {
  import java.net.{InetAddress, UnknownHostException}

  def act() {
    react {
      case (name: String, actor: Actor) =>
        actor ! getIp(name)
        act()
      case "EXIT" =>
        println("Name resolver exiting.")
        // quit
      case msg =>
        println("Unhandled message: " + msg)
        act()
    }
  }

  def getIp(name: String): Option[InetAddress] = {
    try {
      Some(InetAddress.getByName(name))
    } catch {
      case _: UnknownHostException => None
    }
  }

}

object Test extends App {
  /* PinS, Listing 32.2: An actor that calls receive
   */
  def makeEchoActor(): Actor = actor {
    while (true) {
      receive {
        case 'stop =>
          exit()
        case msg =>
          println("received message: " + msg)
      }
    }
  }

  /* PinS, page 696
   */
  def makeIntActor(): Actor = actor {
    receive {
      case x: Int => // I only want Ints
        println("Got an Int: " + x)
    }
  }

  actor {
    self.trapExit = true
    self.link(SillyActor)
    SillyActor.start()
    react {
      case Exit(SillyActor, _) =>
        self.link(SeriousActor)
        SeriousActor.start()
        react {
          case Exit(SeriousActor, _) =>
            val seriousPromise2 = Promise[Boolean]
            // PinS, page 694
            val seriousActor2 = actor {
              for (i <- 1 to 5)
                println("That is the question.")
              seriousPromise2.success(true)
            }

            Await.ready(seriousPromise2.future, 5 seconds)
            val echoActor = makeEchoActor()
            self.link(echoActor)
            echoActor ! "hi there"
            echoActor ! 15
            echoActor ! 'stop
            react {
              case Exit(_, _) =>
                val intActor = makeIntActor()
                intActor ! "hello"
                intActor ! math.Pi
                // only the following send leads to output
                intActor ! 12
            }
        }
    }
  }
}
