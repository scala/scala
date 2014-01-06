import scala.language.experimental.macros
import scala.reflect.macros.BlackboxMacro

trait Bundle extends BlackboxMacro {
  def deferred: Int
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}