import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("it works"))))
    c.Expr[Unit](body)
  }
}