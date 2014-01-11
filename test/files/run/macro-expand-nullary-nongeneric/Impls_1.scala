import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

object Impls {
  def impl(c: Context)(meth: String) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(meth))))
    c.Expr[Unit](body)
  }

  def fooNullary(c: Context) = impl(c)("fooNullary")
  def fooEmpty(c: Context)() = impl(c)("fooEmpty")
  def barNullary(c: Context)(x: c.Expr[Int]) = impl(c)("barNullary")
  def barEmpty(c: Context)(x: c.Expr[Int])() = impl(c)("barEmpty")
}