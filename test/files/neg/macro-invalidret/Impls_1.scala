import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

object Impls {
  def foo1(c: Context) = 2
  def foo2(c: Context) = ru.Literal(ru.Constant(42))
}
