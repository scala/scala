// For the full version of the test, take a look at run/t5923a

import scala.reflect.macros.BlackboxContext
import language.experimental.macros

case class C[T](t: String)
object C {
  implicit def foo[T]: C[T] = macro Macros.impl[T]
}

object Macros {
  def impl[T: c.WeakTypeTag](c: BlackboxContext) = {
    import c.universe._
    reify(C[T](c.literal(weakTypeOf[T].toString).splice))
  }
}