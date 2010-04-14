
import scala.actors.Reactor

case class Ping(from: Reactor[Any])
case object Pong
case object Stop

/**
 * Ping pong example for Reactor.
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

class PingActor(count: Int, pong: Reactor[Any]) extends Reactor[Any] {
  def act() {
    try {
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
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

class PongActor extends Reactor[Any] {
  def act() {
    try {
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
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}
