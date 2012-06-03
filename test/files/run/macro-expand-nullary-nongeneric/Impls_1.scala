import scala.reflect.makro.{Context => Ctx}

object Impls {
  def impl(c: Ctx) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("it works"))))
    c.Expr[Unit](body)
  }

  def fooNullary(c: Ctx) = impl(c)
  def fooEmpty(c: Ctx)() = impl(c)
  def barNullary(c: Ctx)(x: c.Expr[Int]) = impl(c)
  def barEmpty(c: Ctx)(x: c.Expr[Int])() = impl(c)
}