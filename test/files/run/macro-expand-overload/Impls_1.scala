import scala.reflect.makro.{Context => Ctx}

object Impls {
  def impl(c: Ctx)(tag: String, x: c.Expr[_]) = {
    import c.{prefix => prefix}
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(tag)), Literal(Constant(prefix.toString)), x.tree))
    c.Expr[Unit](body)
  }

  def fooObjectString(c: Ctx)(x: c.Expr[_]) = impl(c)("fooObjectString", x)
  def fooObjectInt(c: Ctx)(x: c.Expr[_]) = impl(c)("fooObjectInt", x)
  def fooClassString(c: Ctx)(x: c.Expr[_]) = impl(c)("fooClassString", x)
  def fooClassInt(c: Ctx)(x: c.Expr[_]) = impl(c)("fooClassInt", x)
}