import scala.reflect.macros.Context

class C

object Impls1 {
  def foo[U <: String](c: Context) = c.universe.Ident(c.universe.TypeName("C"))
}

class Bound
class BoundChild extends Bound

object Impls2 {
  def foo[U <: Bound](c: Context) = c.universe.Ident(c.universe.TypeName("C"))
}