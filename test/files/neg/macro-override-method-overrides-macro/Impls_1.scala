import scala.reflect.macros.{Context => Ctx}

object Impls {
  def impl(c: Ctx)(tag: String, x: c.Expr[_]) = {
    import c.{prefix => prefix}
    import c.universe._
    c.Expr[Unit](q"println($tag, ${prefix.toString}, $x)")
  }

  def fooBString(c: Ctx)(x: c.Expr[_]) = impl(c)("fooBString", x)
  def fooBInt(c: Ctx)(x: c.Expr[_]) = impl(c)("fooBInt", x)
  def fooDInt(c: Ctx)(x: c.Expr[_]) = impl(c)("fooDInt", x)
  def fooZString(c: Ctx)(x: c.Expr[_]) = impl(c)("fooZString", x)
}