import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Macro => BlackboxMacro}
import scala.reflect.macros.whitebox.{Macro => WhiteboxMacro}

trait Bundle extends BlackboxMacro with WhiteboxMacro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}