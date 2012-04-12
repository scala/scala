import scala.reflect.makro.Context

object Macros {
  def impl[T: c.TypeTag](c: Context)(foo: c.Expr[T]): c.Expr[Unit] = c.reify { println(c.literal(implicitly[c.TypeTag[T]].toString).eval) }

  def foo[T](foo: T) = macro impl[T]
}