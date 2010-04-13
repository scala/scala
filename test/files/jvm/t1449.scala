import scala.actors.Actor._
import scala.actors.Future
import scala.actors.Futures._
object Test {
  def main(args: Array[String]) {
    val a = actor {
      try {
      react {
        case ft: Future[a] =>
          println(ft())
      }
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
    try {
    val ft = future { 42 }
    a ! ft
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}
