import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("it works"))))
    c.Expr[Unit](body)
  }

  def fooFoo(c: Ctx) = {
    import c.universe._
    val body = Select(Ident(newTermName("Macros")), newTermName("foo"))
    c.Expr[Unit](body)
  }
}