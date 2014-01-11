import scala.reflect.macros.blackbox.Context

object Impls {
  def impl(c: Context)(tag: String, x: c.Expr[_]) = {
    import c.{prefix => prefix}
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(tag)), Literal(Constant(prefix.toString)), x.tree))
    c.Expr[Unit](body)
  }

  def fooBString(c: Context)(x: c.Expr[_]) = impl(c)("fooBString", x)
  def fooBInt(c: Context)(x: c.Expr[_]) = impl(c)("fooBInt", x)
  def fooDInt(c: Context)(x: c.Expr[_]) = impl(c)("fooDInt", x)
  def fooZString(c: Context)(x: c.Expr[_]) = impl(c)("fooZString", x)
}