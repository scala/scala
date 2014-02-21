import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

class Bundle(val c: Context { type Foo <: Int }) {
  import c.universe._
  def impl = q"()"
}

object Macros {
  def foo: Any = macro Bundle.impl
}