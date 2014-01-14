import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    throw new Exception()
  }
  def exception = macro impl
}