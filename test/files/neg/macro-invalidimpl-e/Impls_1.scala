import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Any]) = ???
  def foo(c: Ctx)(x: c.Expr[Any], y: c.Expr[Any]) = ???
}
