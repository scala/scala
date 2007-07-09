package examples.actors

import scala.actors._
import scala.actors.Actor._

object channels extends Application {
  case class Msg(ch1: Channel[int], ch2: Channel[String])

  val a = actor {
    val Ch1 = new Channel[int]
    val Ch2 = new Channel[String]

    b ! Msg(Ch1, Ch2)

    react {
      case x => Console.println("received on int channel: "+x)
      //case y => Console.println("received on String channel: "+y)
    }
  }

  val b = actor {
    react {
      case Msg(ch1, ch2) => ch1 ! 42
    }
  }
}
