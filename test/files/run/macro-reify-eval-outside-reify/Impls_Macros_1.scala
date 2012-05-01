import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    val x1 = c.Expr[Int](c.resetAllAttrs(x.tree))
    c.literal(x1.eval)
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
