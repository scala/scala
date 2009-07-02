import scala.actors._
import scala.actors.Actor._

object Test {

  def main (args: Array[String]) {
    val actors = (1 to 1000).toList map { x => actor { loop { react {
      case x: Array[Int] => reply ("OK"); exit }}}}
    actors foreach { x => x !? new Array[Int] (1000000) }
  }

}
