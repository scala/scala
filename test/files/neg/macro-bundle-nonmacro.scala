import scala.language.experimental.macros

trait Bundle {
  def impl = ???
}

object Macros {
  def foo = Bundle.impl
}