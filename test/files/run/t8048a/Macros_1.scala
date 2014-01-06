import scala.reflect.macros.WhiteboxContext
import scala.language.experimental.macros

object Macros {
  def impl(c: WhiteboxContext) = {
    import c.universe._
    q"if (true) Some(2) else None"
  }

  def foo: Any = macro impl
}