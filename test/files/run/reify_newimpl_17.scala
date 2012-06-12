import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  class C[U] {
    type T = U
    val code = reify {
      List[T](2.asInstanceOf[T])
    }
    println(code.eval)
  }

  try {
    new C[Int]
  } catch {
    case ex: Throwable =>
      println(ex)
  }
}