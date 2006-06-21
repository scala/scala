package actors.examples.counter

import actors.distributed.RemoteActor

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
