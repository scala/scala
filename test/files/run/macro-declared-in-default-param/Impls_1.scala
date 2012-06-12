import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.{prefix => prefix}
    import c.universe._
    val printPrefix = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("prefix = " + prefix))))
    val body = Block(List(printPrefix), Literal(Constant("it works")))
    c.Expr[String](body)
  }
}