import scala.language.experimental.macros
import scala.reflect.macros.blackbox._

object Bundle extends Macro {
  val c: Context = ???
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}