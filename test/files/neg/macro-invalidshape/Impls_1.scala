import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Any]) = ???
}
