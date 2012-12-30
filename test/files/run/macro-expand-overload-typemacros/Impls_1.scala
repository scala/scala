import scala.reflect.macros.{Context => Ctx}

class C(val msg: String)
object Impls {
  def impl(c: Ctx)(tag: String, x: c.Expr[_]) = {
    import c.universe._
    Apply(Ident(TypeName("C")), List(Literal(Constant(tag))))
  }

  def fooObjectString(c: Ctx)(x: c.Expr[_]) = impl(c)("fooObjectString", x)
  def fooObjectInt(c: Ctx)(x: c.Expr[_]) = impl(c)("fooObjectInt", x)
  def fooClassString(c: Ctx)(x: c.Expr[_]) = impl(c)("fooClassString", x)
  def fooClassInt(c: Ctx)(x: c.Expr[_]) = impl(c)("fooClassInt", x)
}