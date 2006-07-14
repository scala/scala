/**
 * @author Philipp Haller <philipp.haller@epfl.ch>
 */

package examples.actors

import scala.actors.Process
import scala.actors.distributed.{RemoteActor,TCP,TcpNode,TcpService}

case class Incr()
case class Value(p: Process)
case class Result(v: int)

class Counter extends RemoteActor {
  override def run(): unit =
    loop(0)

  def loop(value: int): unit = {
    Console.println("Value: " + value)
    receive {
      case Incr() =>
        loop(value + 1)
      case Value(p) =>
        p ! Result(value)
        loop(value)
      case other =>
        loop(value)
    }
  }
}

class CounterUser extends RemoteActor {
  override def run(): unit = {
    alive(TCP())

    val host = java.net.InetAddress.getLocalHost().getHostAddress()
    spawn(TcpNode(host, 9090), classOf[Counter].getName())

    receive {
      case p: Process =>
        // communicate with counter
        Console.println("" + node + ": Sending Incr() to remote Counter (" + p + ")...")
        p ! Incr()
        p ! Incr()
        p ! Value(self)
        receive {
          case Result(v) =>
            Console.println("Received result: " + v)
        }
    }
  }
}

object CounterTest {
  def main(args: Array[String]): unit = {
    val serv = new TcpService(9090)
    serv.start()

    val cu = new CounterUser
    cu.start()
  }
}
