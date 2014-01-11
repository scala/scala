import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(x.tree))
    c.Expr[Unit](body)
  }
}