import scala.reflect.mirror._

object Test extends App {
  class C {
    type T
    val code = reify {
      List[T](2.asInstanceOf[T])
    }
    println(code.eval)
  }

  try {
    new C { val T = Int }
  } catch {
    case ex: Throwable =>
      println(ex)
  }
}