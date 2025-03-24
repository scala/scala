import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._
    Literal(Constant(s"Line: ${x.pos.line}. Width: ${x.pos.end - x.pos.start}."))
  }
  def pos(x: Any): String = macro impl
}
