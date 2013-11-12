import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = x
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
