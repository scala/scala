import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("it works"))))
    c.Expr[Unit](body)
  }

  def fooFoo(c: Context) = {
    import c.universe._
    val body = Select(Ident(TermName("Macros")), TermName("foo"))
    c.Expr[Unit](body)
  }
}