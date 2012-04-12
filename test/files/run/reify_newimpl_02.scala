import scala.reflect.mirror._

object Test extends App {
  {
    var x = 2
    val code = reify {
      x
    }
    println(code.eval)
  }
}