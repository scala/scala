import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

class C(val msg: String)
object Impls {
  def impl(c: Ctx)(meth: String) = {
    import c.universe._
    Apply(Ident(TypeName("C")), List(Literal(Constant(meth))))
  }

  def fooNullary(c: Ctx) = impl(c)("fooNullary")
  def fooEmpty(c: Ctx)() = impl(c)("fooEmpty")
  def barNullary(c: Ctx)(x: c.Expr[Int]) = impl(c)("barNullary")
  def barEmpty(c: Ctx)(x: c.Expr[Int])() = impl(c)("barEmpty")
}