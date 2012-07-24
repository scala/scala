import scala.reflect.macros.{Context => Ctx}

class Macros {
  object Impls {
    def foo(c: Ctx)(x: c.Expr[Any]) = ???
  }

  def foo(x: Any) = macro Impls.foo
}