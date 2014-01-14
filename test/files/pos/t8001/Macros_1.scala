import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def foo: Unit = macro impl
  def impl(c: Context) = {
    import c.universe._
    q"()"
  }
}