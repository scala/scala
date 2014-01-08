import language.experimental.macros
import scala.reflect.macros.Context

object Macro {
  def apply(a: Any): Any = macro impl
  def impl(c: Context)(a: c.Tree): c.Tree = {
    import c.universe._

    q"{$a; true}"
  }
}
