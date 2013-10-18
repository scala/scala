import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo_impl(c: Ctx)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 1")
  }

  def bar_impl(c: Ctx)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 2")
  }

  def quux_impl(c: Ctx)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 3")
  }
}