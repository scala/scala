import scala.reflect.macros.Context

object Impls {
  def impl[T](c: Context) = c.universe.Ident(c.universe.TypeName("Int"))
}
