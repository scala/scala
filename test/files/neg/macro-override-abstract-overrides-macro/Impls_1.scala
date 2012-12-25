import scala.reflect.macros.Context

class B
class C extends B

object Impls {
  def impl(c: Context) = c.universe.Ident(c.universe.TypeName("B"))
}
