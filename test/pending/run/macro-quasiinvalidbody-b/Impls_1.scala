import scala.reflect.macros.blackbox.Context

trait ImplContainer {
  object Impls {
    def foo(c: Context)(x: c.Expr[Any]) = x
  }
}