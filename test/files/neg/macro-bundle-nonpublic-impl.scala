import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

class Macros(val c: Context) {
  import c.universe._
  private def impl = q"()"
}

object Macros {
  def foo: Any = macro Macros.impl
}