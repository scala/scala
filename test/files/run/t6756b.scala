import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

trait A { type B }

object Test {
  def expr[T <: A : WeakTypeTag] = reify {
    null.asInstanceOf[T#B]
  }

  def main(args: Array[String]): Unit = {
    assert(expr[A].eval == null)
  }
}
