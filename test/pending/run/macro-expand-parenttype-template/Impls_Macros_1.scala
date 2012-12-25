import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.enclosingTemplate match {
      case Template(parents, self, defs) =>
        Template(List(Ident(TypeName("AnyRef"))), self, defs)
    }
  }

  type Foo = macro impl
}
