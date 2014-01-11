import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context) = {
    import c.{prefix => prefix}
    import c.universe._
    val printPrefix = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("prefix = " + prefix))))
    val body = Block(List(printPrefix), Literal(Constant("it works")))
    c.Expr[String](body)
  }
}