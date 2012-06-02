import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo_impl(c: Ctx)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(Apply(Select(x.tree, newTermName("$plus")), List(Literal(Constant(1)))))
  }

  def bar_impl(c: Ctx)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(Apply(Select(x.tree, newTermName("$plus")), List(Literal(Constant(2)))))
  }

  def quux_impl(c: Ctx)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(Apply(Select(x.tree, newTermName("$plus")), List(Literal(Constant(3)))))
  }
}