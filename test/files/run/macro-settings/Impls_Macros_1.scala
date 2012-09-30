import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = c.universe.reify {
    println(c.literal(c.settings.toString).splice)
  }
}

object Macros {
  def foo = macro Impls.impl
}