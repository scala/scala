import scala.reflect.macros.Context

object Macros {
  def impl[T](c: Context) = c.literalUnit
  def foo[T] = macro impl[T]
}