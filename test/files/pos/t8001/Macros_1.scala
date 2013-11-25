import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def foo = macro impl
  def impl(c: BlackboxContext) = {
    import c.universe._
    q"()"
  }
}