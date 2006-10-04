package examples.actors

import scala.actors.Reactor
import scala.actors.Actor._

abstract class PingMessage
case class MsgStart() extends PingMessage
case class MsgPingInit(count: int, pong: Pong) extends PingMessage
case class MsgSendPing extends PingMessage
case class MsgPong(sender: Pong) extends PingMessage

abstract class PongMessage
case class MsgPing(sender: Ping) extends PongMessage
case class MsgStop() extends PongMessage

object PingPong {
  def main(args : Array[String]): Unit = {
    val ping = new Ping
    ping.start
    val pong = new Pong
    pong.start
    ping ! MsgPingInit(100000, pong)
    ping ! MsgStart
  }
}


class Ping extends Reactor {
  def act(): unit = {
    loop(0, null)
  }
  def loop(pingsLeft: int, pong: Pong): unit = {
    react {
      case MsgPingInit(count, pong) => {
        System.out.println("Ping: Initializing with count:"+count+":"+pong)
        loop(count, pong)
      }
      case MsgStart() => {
        System.out.println("Ping: starting.")
        pong ! MsgPing(this)
        loop(pingsLeft-1, pong)
      }
      case MsgSendPing() => {
        pong ! MsgPing(this)
        loop(pingsLeft-1, pong)
      }
      case MsgPong(pidS) => {
        if (pingsLeft % 100 == 0) {
          System.out.println("Ping: pong from: "+pidS)
        }
        if (pingsLeft > 0)
          this ! MsgSendPing()
        else {
          System.out.println("Ping: Stop.")
          pong ! MsgStop()
        }
        loop(pingsLeft, pong)
      }
    }
  }
}

class Pong extends Reactor {
  def act(): unit = {
    loop(0)
  }

  def loop(pongCount: int): unit = {
    react {
      case MsgPing(pidPing) => {
        if (pongCount % 100 == 0) {
          System.out.println("Pong: ping:"+pongCount+" from: "+pidPing)
        }
        pidPing ! MsgPong(this)
        loop(pongCount+1)
      }
      case MsgStop() => {
        System.out.println("Pong: Stop.")
        System.exit(0)
      }
    }
  }
}
