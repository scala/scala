import scala.reflect.macros.blackbox.Context

object Macros {
  def foo[T](s: T) = macro Impls.foo[List[T]]

  object Impls {
    def foo[T](c: Context)(s: c.Expr[T]) = c.universe.reify {
      List(s.splice)
    }
  }
}