import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait Bundle {
  val c: Context = ???
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}