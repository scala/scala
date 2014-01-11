import scala.reflect.macros.blackbox.Context

object Macros {
  def foo[T](s: String) = macro Impls.foo[T]

  object Impls {
    def foo[T: c.WeakTypeTag](c: Context)(s: c.Expr[T]) = c.universe.reify {
      Array(s.splice)
    }
  }
}