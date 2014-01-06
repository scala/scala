import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext) = {
    throw new Exception()
  }
  def exception = macro impl
}