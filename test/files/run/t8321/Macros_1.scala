import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

class Bundle(val c: Context) {
  import c.universe._
  def impl = q"new { val x = 2 }"
}

object Macros {
  def foo: Any = macro Bundle.impl
}