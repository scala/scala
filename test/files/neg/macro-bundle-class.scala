import scala.language.experimental.macros
import scala.reflect.macros.Macro
import scala.reflect.macros.Context

class Bundle(val c: Context) extends Macro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}