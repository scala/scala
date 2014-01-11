import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Macro

trait Bundle extends Macro {
  def deferred: Int
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}