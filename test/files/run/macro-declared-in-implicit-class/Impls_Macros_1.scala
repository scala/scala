import scala.reflect.makro.{Context => Ctx}

object Impls {
  def toOptionOfInt(c: Ctx) = {
    import c.{prefix => prefix}
    import c.universe._
    val printPrefix = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("prefix = " + prefix))))
    val body = Block(printPrefix, Apply(Ident(definitions.SomeModule), List(Select(Select(prefix.tree, newTermName("x")), newTermName("toInt")))))
    c.Expr[Option[Int]](body)
  }
}

object Macros {
  implicit def foo(x: String): Foo = new Foo(x)

  class Foo(val x: String) {
    def toOptionOfInt = macro Impls.toOptionOfInt
  }
}