import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def foo[U <: String](c: Ctx) = c.universe.Ident(c.universe.TypeName("C"))
}
