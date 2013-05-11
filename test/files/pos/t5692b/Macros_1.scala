import scala.reflect.macros.Context

object Macros {
  def impl[T, U](c: Context) = c.literalUnit
  def foo[T, U] = macro impl[T, U]
}