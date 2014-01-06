import scala.language.experimental.macros
import scala.reflect.macros.BlackboxMacro
import scala.reflect.macros.BlackboxContext

trait Bundle extends BlackboxMacro {
  val c: BlackboxContext = ???
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}