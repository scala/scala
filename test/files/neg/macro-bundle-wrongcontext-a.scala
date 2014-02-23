import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

abstract class MyContext extends Context

class Bundle(val c: MyContext) {
  import c.universe._
  def impl = q"()"
}

object Macros {
  def foo: Any = macro Bundle.impl
}