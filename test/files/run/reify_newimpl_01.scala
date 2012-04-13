import scala.reflect.mirror._

object Test extends App {
  {
    val x = 2
    val code = reify {
      x
    }
    println(code.eval)
  }
}