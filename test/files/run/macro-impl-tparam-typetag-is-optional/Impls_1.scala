import scala.reflect.macros.blackbox.Context

object Impls {
  def foo[U](c: Context) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("don't know U"))))
    c.Expr[Unit](body)
  }
}