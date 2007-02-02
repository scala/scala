package examples.actors

import scala.actors.Actor
import scala.actors.Actor._

abstract class PingMessage
case class MsgPingInit(count: int, pong: Actor) extends PingMessage
case object MsgStart extends PingMessage
case object MsgSendPing extends PingMessage
case object MsgPong extends PingMessage

abstract class PongMessage
case object MsgPing extends PongMessage
case object MsgStop extends PongMessage

object pingpong {
  def main(args : Array[String]) {
    val ping = new Ping
    ping.start
    val pong = new Pong
    pong.start
    ping ! MsgPingInit(100000, pong)
    ping ! MsgStart
  }
}

class Ping extends Actor {
  def act() {
    var pingsLeft = 0
    var pong: Actor = null
    loop {
      react {
        case MsgPingInit(count, png) =>
          Console.println("Ping: Initializing with count "+count+": "+png)
          pingsLeft = count
          pong = png
        case MsgStart =>
          Console.println("Ping: starting.")
          pong ! MsgPing
          pingsLeft = pingsLeft - 1
        case MsgSendPing =>
          pong ! MsgPing
          pingsLeft = pingsLeft - 1
        case MsgPong =>
          if (pingsLeft % 100 == 0)
            Console.println("Ping: pong from: "+sender)
          if (pingsLeft > 0)
            self ! MsgSendPing
          else {
            Console.println("Ping: Stop.")
            pong ! MsgStop
            exit("stop")
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
        case MsgPing =>
          if (pongCount % 100 == 0)
            Console.println("Pong: ping "+pongCount+" from "+sender)
          sender ! MsgPong
          pongCount = pongCount + 1
        case MsgStop =>
          Console.println("Pong: Stop.")
          exit("stop")
      }
    }
  }
}
