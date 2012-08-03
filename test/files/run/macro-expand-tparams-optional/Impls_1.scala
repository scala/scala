import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U](c: Ctx) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("don't know U"))))
    c.Expr[Unit](body)
  }
}