import scala.reflect.macros.blackbox.Context

object Impls {
  def toOptionOfInt(c: Context) = {
    import c.{prefix => prefix}
    import c.universe._
    val printPrefix = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("prefix = " + prefix))))
    val body = Block(List(printPrefix), Apply(Ident(definitions.SomeModule), List(Select(Select(prefix.tree, TermName("x")), TermName("toInt")))))
    c.Expr[Option[Int]](body)
  }
}

object Macros {
  implicit def foo(x: String): Foo = new Foo(x)

  class Foo(val x: String) {
    def toOptionOfInt = macro Impls.toOptionOfInt
  }
}