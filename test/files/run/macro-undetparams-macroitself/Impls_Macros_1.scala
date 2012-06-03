import scala.reflect.runtime.universe._
import scala.reflect.makro.Context

object Macros {
  def impl[T: c.TypeTag](c: Context)(foo: c.Expr[T]): c.Expr[Unit] = c.reify { println(c.literal(implicitly[c.TypeTag[T]].toString).splice) }

  def foo[T](foo: T) = macro impl[T]
}