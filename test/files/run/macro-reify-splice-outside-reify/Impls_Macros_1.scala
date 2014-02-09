import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]) = {
    import c.universe._
    val x1 = c.Expr[Int](c.untypecheck(x.tree))
    c.Expr[Int](Literal(Constant(c.eval(x1))))
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
