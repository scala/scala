import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(y: c.Expr[Int], x: c.Expr[Int]) = ???
}

object Macros {
  def foo(x: Int, y: Int) = macro Impls.foo
}
