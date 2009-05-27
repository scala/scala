import scala.actors.Futures

/* Test that unevaluated futures do not prevent program termination */

object Test {
  def main(args: Array[String]) {
    val meaningOfLife = Futures.future {
      Thread.sleep(5000) // pretend this is a harder problem than it is
      println("I have the answer!")
      42
    }
    println("I can't wait that long, bye.")
  }
}
