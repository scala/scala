package examples.actors

import scala.actors.Actor._
import scala.actors.RemoteActor._
import scala.actors.{Actor,TcpNode}

case object Incr
case object Value
case class Result(v: int)

object RemoteCounter extends Application {
  actor {
    def loop(value: int): unit = {
      Console.println("Value: " + value)
      receive {
        case Incr  => loop(value + 1)
        case Value => { sender ! Result(value); loop(value) }
        case other => loop(value)
      }
    }

    alive(9010)
    register('counter, self)
    loop(0)
  }

  actor {
    val c = select(TcpNode("127.0.0.1", 9010), 'counter)

    c ! Incr
    c ! Incr

    try { Thread.sleep(1000) } catch {
      case ie: InterruptedException =>
    }

    c ! Value
    receive {
      case Result(v) => {
        Console.println("Received result: " + v)
        sender ! Incr
      }
    }
  }
}
