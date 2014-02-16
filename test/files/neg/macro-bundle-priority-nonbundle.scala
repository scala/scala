import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

class Macros(val c: scala.reflect.api.Universe) {
  def impl(x: c.Tree) = ???
}

object Macros {
  def impl(c: Context)(x: c.Tree) = ???
}

object Test extends App {
  def foo: Unit = macro Macros.impl
}