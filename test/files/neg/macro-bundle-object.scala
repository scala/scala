import scala.language.experimental.macros
import scala.reflect.macros.{BlackboxMacro, BlackboxContext}

object Bundle extends BlackboxMacro {
  val c: BlackboxContext = ???
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}