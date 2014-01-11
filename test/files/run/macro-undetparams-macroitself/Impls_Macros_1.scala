import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl[T: c.WeakTypeTag](c: Context)(foo: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    reify { println(c.Expr[String](Literal(Constant(implicitly[c.WeakTypeTag[T]].toString))).splice) }
  }

  def foo[T](foo: T) = macro impl[T]
}