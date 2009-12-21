import scala.actors.Actor._
import scala.actors.Future
import scala.actors.Futures._
object Test {
  def main(args: Array[String]) {
    val a = actor {
      react {
        case ft: Future[a] =>
          println(ft())
      }
    }
    val ft = future { 42 }
    a ! ft
  }
}
