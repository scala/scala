import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: Int) = ???
}

object Macros {
  def foo(x: Int): Nothing = macro Impls.foo
}
