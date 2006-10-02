package examples.actors

import scala.actors.Actor._
import scala.actors.Channel

object Input extends Application {

  var in = new Channel[Pair[int, int]]

  actor(in) {
    in.receive {
      case Pair(x, y) => reply(x + y)
    }
  }

  actor {
    scala.Console.println(in !? Pair(40, 2))
  }
}
