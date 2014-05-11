import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) {
  def impl: c.Tree = ???
}

object Macros {
  def foo: Any = macro Impl.impl
}