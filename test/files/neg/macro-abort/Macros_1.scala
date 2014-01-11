import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    c.abort(c.enclosingPosition, "aborted")
  }
  def abort = macro impl
}