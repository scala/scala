import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Bundle {
  val c: Context = ???
  def impl = ???
}

object Macros {
  def foo: Any = macro Bundle.impl
}
