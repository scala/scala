import scala.language.experimental.macros
import scala.reflect.macros.Macro
import scala.reflect.macros.Context

object Bundle extends Macro {
  val c: Context = ???
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}