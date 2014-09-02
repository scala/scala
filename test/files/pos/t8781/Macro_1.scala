import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val name = TypeName(c.freshName())
    q"class $name extends T; new $name"
  }
  def fresh: Any = macro impl
}

trait T
