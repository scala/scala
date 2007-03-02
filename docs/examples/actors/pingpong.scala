package examples.actors

import scala.actors.Actor
import scala.actors.Actor._

case object SendPing
case object Ping
case object Pong
case object Stop

class Ping(count: int, pong: Actor) extends Actor {
  def act() {
    var pingsLeft = count
    loop {
      react {
        case SendPing =>
          pong ! Ping
          pingsLeft = pingsLeft - 1
        case Pong =>
          if (pingsLeft % 1000 == 0)
            Console.println("Ping: pong")
          if (pingsLeft > 0)
            self ! SendPing
          else {
            Console.println("Ping: stop")
            pong ! Stop
            exit('stop)
          }
      }
    }
  }
}

class Pong extends Actor {
  def act() {
    var pongCount = 0
    loop {
      react {
        case Ping =>
          if (pongCount % 1000 == 0)
            Console.println("Pong: ping "+pongCount)
          sender ! Pong
          pongCount = pongCount + 1
        case Stop =>
          Console.println("Pong: stop")
          exit('stop)
      }
    }
  }
}

object pingpong extends Application {
  val pong = new Pong
  val ping = new Ping(100000, pong)
  ping.start
  pong.start
  ping ! SendPing
}
