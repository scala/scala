import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def impl(c: Ctx)(meth: String) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(meth))))
    c.Expr[Unit](body)
  }

  def fooNullary(c: Ctx) = impl(c)("fooNullary")
  def fooEmpty(c: Ctx)() = impl(c)("fooEmpty")
  def barNullary(c: Ctx)(x: c.Expr[Int]) = impl(c)("barNullary")
  def barEmpty(c: Ctx)(x: c.Expr[Int])() = impl(c)("barEmpty")
}