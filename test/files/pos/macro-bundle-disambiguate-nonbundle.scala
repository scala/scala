import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

class Macros(val c: Context) {
  def impl(x: c.Tree) = ???
}

object Macros {
  def impl(c: Context) = ???
}

object Test extends App {
  def foo: Unit = macro Macros.impl
}