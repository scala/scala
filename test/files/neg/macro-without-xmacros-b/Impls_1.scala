import scala.reflect.macros.blackbox.Context

object Impls {
  def foo_impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 1")
  }

  def bar_impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 2")
  }

  def quux_impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 3")
  }
}