import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int], y: c.Expr[Int]) = {
    import c.universe._
    val sum = Apply(Select(x.tree, TermName("$minus")), List(y.tree))
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(sum))
    Expr[Unit](body)
  }
}