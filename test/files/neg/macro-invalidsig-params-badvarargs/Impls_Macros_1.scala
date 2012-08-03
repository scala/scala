import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(xs: c.Expr[Int]*) = ???
}

object Macros {
  def foo(x: Int, y: Int) = macro Impls.foo
}
