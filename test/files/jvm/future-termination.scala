
/* Test that unevaluated futures do not prevent program termination */


@deprecated("Suppress warnings", since="2.11")
object Test {
  import scala.actors.Futures
  def main(args: Array[String]) {
    try {
    val meaningOfLife = Futures.future {
      Thread.sleep(5000) // pretend this is a harder problem than it is
      println("I have the answer!")
      42
    }
    println("I can't wait that long, bye.")
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}
