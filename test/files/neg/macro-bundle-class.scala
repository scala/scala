import scala.language.experimental.macros
import scala.reflect.macros.BlackboxMacro
import scala.reflect.macros.BlackboxContext

class Bundle(val c: BlackboxContext) extends BlackboxMacro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}