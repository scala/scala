import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.{prefix => prefix}
    import c.universe._
    val printPrefix = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("prefix = " + prefix))))
    val body = Block(printPrefix, Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("it works")))))
    c.Expr[Unit](body)
  }
}