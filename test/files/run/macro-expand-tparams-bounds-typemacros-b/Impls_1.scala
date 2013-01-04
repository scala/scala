import scala.reflect.macros.{Context => Ctx}

class Bound
class C
object Impls {
  def foo[U <: Bound](c: Ctx) = c.universe.Ident(c.universe.TypeName("C"))
}