import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]) = x
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
