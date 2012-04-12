import scala.reflect.mirror._

object Test extends App {
  val outer1 = {
    val x = 2
    reify{x}
  }

  val outer2 = {
    val x = 3
    reify{x}
  }

  val code = reify{
    val x = 4
    x + outer1.eval + outer2.eval
  }

  println(code.eval)
}
