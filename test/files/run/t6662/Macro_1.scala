import language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Demo {
  def id[T](a: T): T = macro idImpl[T]

  def idImpl[T: c.WeakTypeTag](c: BlackboxContext)(a: c.Expr[T]): c.Expr[T] = a
}
