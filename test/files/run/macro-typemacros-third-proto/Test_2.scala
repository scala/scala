import scala.concurrent._
import scala.util._
import ExecutionContext.Implicits.global
import Macros._

object Test extends App {
  // TODO: make sure this test passes if not only compiled, but also ran
  // so far I haven't been able to make SIP-14 stuff work reliably
  def wrapper() = {
    class D extends Lifter {
      def x = 2
    }

    val d = new D
    d.asyncX onComplete {
      case Success(x) => println(x)
      case Failure(_) => println("failed")
    }
  }
}
