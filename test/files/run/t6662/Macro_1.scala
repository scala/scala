import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Demo {
  def id[T](a: T): T = macro idImpl[T]

  def idImpl[T: c.WeakTypeTag](c: Context)(a: c.Expr[T]): c.Expr[T] = a
}
