import scala.language.experimental.macros

class Bundle {
  def impl = ???
}

object Macros {
  def foo = Bundle.impl
}