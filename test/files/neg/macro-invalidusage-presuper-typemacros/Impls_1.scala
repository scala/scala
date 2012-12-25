import scala.reflect.macros.Context

class C

object Impls {
  def impl(c: Context) = c.universe.Ident(c.universe.TypeName("C"))
}