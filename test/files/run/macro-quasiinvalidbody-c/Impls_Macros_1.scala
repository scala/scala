// scalac: -language:experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  object Impls {
    def foo(c: Context)(x: c.Expr[Any]) = x
  }

  def foo(x: Any) = macro Impls.foo
}
