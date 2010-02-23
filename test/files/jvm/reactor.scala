
import scala.actors.Reactor

case class Ping(from: Reactor)
case object Pong
case object Stop

/**
 * Ping pong example for OutputChannelActor.
 *
 * @author  Philipp Haller
 */
object Test {
  def main(args: Array[String]) {
    val pong = new PongActor
    val ping = new PingActor(100000, pong)
    ping.start
    pong.start
  }
}

class PingActor(count: Int, pong: Reactor) extends Reactor {
  def act() {
    var pingsLeft = count - 1
    pong ! Ping(this)
    loop {
      react {
        case Pong =>
          if (pingsLeft % 10000 == 0)
            println("Ping: pong")
          if (pingsLeft > 0) {
            pong ! Ping(this)
            pingsLeft -= 1
          } else {
            println("Ping: stop")
            pong ! Stop
            exit()
          }
      }
    }
  }
}

class PongActor extends Reactor {
  def act() {
    var pongCount = 0
    loop {
      react {
        case Ping(from) =>
          if (pongCount % 10000 == 0)
            println("Pong: ping "+pongCount)
          from ! Pong
          pongCount += 1
        case Stop =>
          println("Pong: stop")
          exit()
      }
    }
  }
}
