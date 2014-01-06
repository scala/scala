import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext) = {
    c.abort(c.enclosingPosition, "aborted")
  }
  def abort = macro impl
}