import scala.actors.{Futures, TIMEOUT}
import scala.actors.Actor._

object Test {

  def compute(): Option[Boolean] = {
    val fts = for (j <- 0 until 5) yield Futures.future {
      receiveWithin (100) {
        case TIMEOUT => true
        case other   => false
      }
    }
    val done = Futures.awaitAll(2000, fts.toArray: _*) // list to array, as varargs
    if (done.contains(None))
      None
    else
      Some(true)
  }

  def main(args:Array[String]) : Unit = {
    val ft = Futures.future {
      val format = new java.text.DecimalFormat("000.00'ms'")
      var iter = 1
      val done = 11
      while (iter < done) {
        val start = System.nanoTime()
        val result = compute()
        val time = System.nanoTime() - start
        result match {
          case Some(result) =>
            //printf("Iteration %2d succeeded after %s %n", iter, format.format(time / 1e6))
            printf("Iteration %2d succeeded%n", iter)
            iter += 1
          case None =>
            printf(">>>> Iteration %2d failed after %s <<<<< %n", iter, format.format(time / 1e6))
            iter = done
        }
      }
    }
    ft()
  }

}
