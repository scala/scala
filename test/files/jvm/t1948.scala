import scala.actors._
import scala.actors.Actor._

object Test {

  def main (args: Array[String]) {
    val actors = (1 to 1000).toList map { x => actor {
      try {
      loop { react {
      case x: Array[Int] => reply ("OK"); exit }}
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    } }
    try {
    actors foreach { x => x !? new Array[Int] (1000000) }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }

}
