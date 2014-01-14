import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    q"if (true) Some(2) else None"
  }

  def foo: Any = macro impl
}