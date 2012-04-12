import scala.reflect.mirror._

object Test extends App {
  object C {
    type T = Int
    val code = reify {
      List[C.T](2)
    }
    println(code.eval)
  }

  C
}