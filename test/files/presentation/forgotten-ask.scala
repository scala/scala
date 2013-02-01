import scala.tools.nsc.interactive._
import tests._

/** Test that no ask calls are left unanswered after a compiler has shut down. */
object Test extends InteractiveTest {
  import compiler._

  def askItem(): Response[Unit] = {
    compiler.askForResponse { () =>
      Thread.sleep(100)
    }
  }

  final val Timeout = 5000 //ms

  override def main(args: Array[String]) {
    val item1 = askItem()

    compiler.askShutdown()

    Thread.sleep(1000) // wait a bit, the compiler is shutting down
    val item2 = askItem()

    item1.get(Timeout) match {
      case None => println("TIMEOUT")
      case _ =>
    }
    item2.get(Timeout) match {
      case None => println("TIMEOUT")
      case _ =>
    }
  }
}