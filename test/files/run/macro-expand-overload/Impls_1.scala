import scala.reflect.macros.blackbox.Context

object Impls {
  def impl(c: Context)(tag: String, x: c.Expr[_]) = {
    import c.{prefix => prefix}
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(tag)), Literal(Constant(prefix.toString)), x.tree))
    c.Expr[Unit](body)
  }

  def fooObjectString(c: Context)(x: c.Expr[_]) = impl(c)("fooObjectString", x)
  def fooObjectInt(c: Context)(x: c.Expr[_]) = impl(c)("fooObjectInt", x)
  def fooClassString(c: Context)(x: c.Expr[_]) = impl(c)("fooClassString", x)
  def fooClassInt(c: Context)(x: c.Expr[_]) = impl(c)("fooClassInt", x)
}