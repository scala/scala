import scala.reflect.macros.Context

class C

object Impls {
  def impl0(c: Context) = c.universe.Ident(c.universe.TypeName("C"))
  def impl1(c: Context)(x: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("C"))
  def impl2(c: Context)(x: c.Expr[String]) = c.universe.Ident(c.universe.TypeName("C"))
}
