import scala.reflect.makro.{Context => Ctx}

class Impls {
  def foo(c: Ctx)(x: c.Expr[Any]) = ???
}
