import scala.language.experimental.macros
import scala.reflect.macros.{BlackboxMacro, WhiteboxMacro}

trait Bundle extends BlackboxMacro with WhiteboxMacro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}