import scala.reflect.macros.Context

object Impls {
  def tm(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("C"))
  def tm_a(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("A"))
  def tm_l(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("List"))
  def tm_t(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("T"))
  def tm_d(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("D"))
}

object Macros {
  type TM(x: Int)(y: Int) = macro Impls.tm
  type TM_A(x: Int)(y: Int) = macro Impls.tm_a
  type TM_L(x: Int)(y: Int) = macro Impls.tm_l
  type TM_T(x: Int)(y: Int) = macro Impls.tm_t
  type TM_D(x: Int)(y: Int) = macro Impls.tm_d
}

class A extends scala.annotation.StaticAnnotation
class B
trait C
class D
