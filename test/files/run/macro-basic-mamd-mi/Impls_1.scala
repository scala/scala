import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(1)))))
  }

  def bar(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(2)))))
  }

  def quux(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    val body = Apply(Select(x.tree, TermName("$plus")), List(Literal(Constant(3))))
    c.Expr[Int](body)
  }
}