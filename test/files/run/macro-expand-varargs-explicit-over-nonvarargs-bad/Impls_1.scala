import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(xs: c.Expr[Int]*) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), xs.map(_.tree).toList)
    c.Expr[Unit](body)
  }
}