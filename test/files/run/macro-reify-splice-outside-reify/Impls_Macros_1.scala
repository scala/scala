import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    val x1 = c.Expr[Int](c.resetAllAttrs(x.tree))
    c.Expr[Int](Literal(Constant(c.eval(x1))))
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
