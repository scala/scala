import scala.reflect.macros.Context

object Impls {
  def tm[T](c: Context) = c.universe.Ident(c.universe.TypeName("C"))
  def tm_a[T](c: Context) = c.universe.Ident(c.universe.TypeName("A"))
  def tm_l[T](c: Context) = c.universe.Ident(c.universe.TypeName("List"))
  def tm_t[T](c: Context) = c.universe.Ident(c.universe.TypeName("T"))
  def tm_d[T](c: Context) = c.universe.Ident(c.universe.TypeName("D"))
}

object Macros {
  type TM[T] = macro Impls.tm[T]
  type TM_A[T] = macro Impls.tm_a[T]
  type TM_L[T] = macro Impls.tm_l[T]
  type TM_T[T] = macro Impls.tm_t[T]
  type TM_D[T] = macro Impls.tm_d[T]
}

class A extends scala.annotation.StaticAnnotation
class B
trait C
class D
