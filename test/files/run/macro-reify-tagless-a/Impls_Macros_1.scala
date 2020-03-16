import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def foo[T](s: T): List[T] = macro Impls.foo[T]

  object Impls {
    def foo[T](c: Context)(s: c.Expr[T]) = c.universe.reify {
      List[T](s.splice)
    }
  }
}
