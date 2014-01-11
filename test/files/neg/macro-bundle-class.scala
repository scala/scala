import scala.language.experimental.macros
import scala.reflect.macros.blackbox._

class Bundle(val c: Context) extends Macro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}