
import scala.language.postfixOps
import scala.language.reflectiveCalls

object Test extends App {
  val x = new {
    def > = 1
  }

  println(x>)
}
