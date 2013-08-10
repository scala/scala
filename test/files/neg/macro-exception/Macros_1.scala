import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = {
    throw new Exception()
  }
  def exception = macro impl
}