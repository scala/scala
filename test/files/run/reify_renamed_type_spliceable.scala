import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

abstract class C {
  type T >: Null
}

object Test extends App {
  def foo(c: C) = {
    import c.{T => U}
    reify {
      val x: U = null
    }
  }

  val expr = foo(new C {
    type T = AnyRef
  })

  println(expr.eval)
}