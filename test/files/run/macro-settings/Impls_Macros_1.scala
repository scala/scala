import scala.reflect.makro.Context

object Impls {
  def impl(c: Context) = c.reify {
    println(c.literal(c.settings.toString).splice)
  }
}

object Macros {
  def foo = macro Impls.impl
}