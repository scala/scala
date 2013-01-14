import scala.reflect.macros.Context

class B
class C extends B

object Impls {
  def impl_b(c: Context) = c.universe.Ident(c.universe.TypeName("B"))
  def impl_c(c: Context) = c.universe.Ident(c.universe.TypeName("C"))
  def impl_c1(c: Context)() = c.universe.Ident(c.universe.TypeName("C"))
  def impl_c2(c: Context)(x: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("C"))
  def impl_c3(c: Context)(x: c.Expr[String]) = c.universe.Ident(c.universe.TypeName("C"))
}
