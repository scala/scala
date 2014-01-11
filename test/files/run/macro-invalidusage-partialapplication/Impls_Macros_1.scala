import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = {
    import c.universe._
    val sum = Apply(Select(x.tree, TermName("$plus")), List(y.tree))
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(sum))
    c.Expr[Unit](body)
  }
}

object Macros {
  def foo(x: Int)(y: Int) = macro Impls.foo
}