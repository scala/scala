import scala.reflect.mirror._

object Test extends App {
  object C {
    type T = Int
    val c = C
    val code = reify {
      List[c.T](2)
    }
    println(code.eval)
  }

  C
}