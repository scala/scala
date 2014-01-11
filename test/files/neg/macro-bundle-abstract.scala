import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

abstract class Bundle(c: Context) {
  def deferred: Int
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}