import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    val x1 = c.Expr[Int](c.resetAllAttrs(x.tree))
// was:    c.literal(x1.splice)
    c.literal(c.eval(x1))
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
