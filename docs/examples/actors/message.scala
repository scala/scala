package examples.actors

import scala.actors._; import scala.actors.Actor._

object message {
  def main(args: Array[String]) = {
    val n = try { Integer.parseInt(args(0)) }
    catch {
      case _ =>
        scala.Console.println("Usage: examples.actors.Message <n>")
        Predef.exit
    }
    val nActors = 500
    val finalSum = n * nActors
    Scheduler.impl = new SingleThreadedScheduler

    def beh(next: Actor, sum: int): unit =
      react {
        case value: int =>
          val j = value + 1; val nsum = sum + j
          if (next == null && nsum >= finalSum)
            Console.println(nsum)
          else {
            if (next != null) next ! j
            beh(next, nsum)
          }
      }

    def actorChain(i: Int, a: Actor): Actor =
      if (i > 0) actorChain(i-1, actor(beh(a, 0))) else a

    val firstActor = actorChain(nActors, null)
    var i = n; while (i > 0) { firstActor ! 0; i = i-1 }
  }
}
