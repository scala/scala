import scala.reflect.runtime.universe._
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl[T: c.WeakTypeTag](c: BlackboxContext)(foo: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    reify { println(c.Expr[String](Literal(Constant(implicitly[c.WeakTypeTag[T]].toString))).splice) }
  }

  def foo[T](foo: T) = macro impl[T]
}