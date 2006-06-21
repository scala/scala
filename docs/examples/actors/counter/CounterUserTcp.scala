package actors.examples.counter

import actors.multi.Pid
import actors.distributed.RemoteActor
import actors.distributed.TCP
import actors.distributed.TcpNode
import actors.distributed.TcpService

class CounterUser extends RemoteActor {
  override def run(): unit = {
    alive(TCP())

    spawn(TcpNode("127.0.0.1", 9090), "actors.examples.counter.Counter")

    receive {
      case p: Pid =>
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

object Main {
  def main(args: Array[String]): unit = {
    val serv = new TcpService(9090)
    serv.start()

    val cu = new CounterUser
    cu.start()
  }
}
