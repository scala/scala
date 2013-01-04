import scala.reflect.macros.Context

class C

object Impls {
  def impl[T](c: Context) = c.universe.Ident(c.universe.TypeName("C"))
}
