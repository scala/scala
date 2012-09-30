import scala.reflect.macros.{Context => Ctx}

class Impls {
  def foo(c: Ctx)(x: c.Expr[Any]) = ???
}
