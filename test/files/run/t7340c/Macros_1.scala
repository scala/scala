import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = c.literalUnit
}