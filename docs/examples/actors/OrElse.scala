package examples.actors

import scala.actors._
import scala.actors.Actor._

object OrElse {
  def main(args: Array[String]) = {
    actor {
      val b1 = new Channel[int]
      val b2 = new Channel[int]
      b2 ! 42
      val item = { b1.receive {case any => any}
                 } orElse {
                   b2.receive {case any => any} }

      Console.println("" + item)
    }
  }
}
