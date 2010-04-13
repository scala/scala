import scala.actors.Futures._
import scala.actors.Actor._

object Test {
  def main(args: Array[String]) {
    try {
    val ft1 = future { reactWithin(10000) {
      case _ => println("FAIL")
    } }

    val ft2 = future { reactWithin(20000) {
      case _ => println("FAIL")
    } }

    val res = awaitAll(0, ft1, ft2)
    println("OK")
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}
