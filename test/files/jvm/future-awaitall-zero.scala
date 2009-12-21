import scala.actors.Futures._
import scala.actors.Actor._

object Test {
  def main(args: Array[String]) {
    val ft1 = future { reactWithin(1000) {
      case _ => println("FAIL")
    } }

    val ft2 = future { reactWithin(2000) {
      case _ => println("FAIL")
    } }

    val res = awaitAll(0, ft1, ft2)
    println("OK")
  }
}
