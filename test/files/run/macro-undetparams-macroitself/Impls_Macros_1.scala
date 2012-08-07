import scala.reflect.runtime.universe._
import scala.reflect.macros.Context

object Macros {
  def impl[T: c.AbsTypeTag](c: Context)(foo: c.Expr[T]): c.Expr[Unit] = c.universe.reify { println(c.literal(implicitly[c.AbsTypeTag[T]].toString).splice) }

  def foo[T](foo: T) = macro impl[T]
}