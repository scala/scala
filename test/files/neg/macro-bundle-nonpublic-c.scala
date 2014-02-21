import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

class Macros(c: Context) {
  import c.universe._
  def impl = q"()"
}

object Macros {
  def foo: Any = macro Macros.impl
}