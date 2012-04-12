import scala.reflect.mirror._

object Test extends App {
  {
    val code = reify {
      var x = 2
      reify{x}.eval
    }
    println(code.eval)
  }
}