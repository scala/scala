import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = c.literalUnit
}