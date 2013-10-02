import scala.reflect.macros.BlackboxContext
import scala.reflect.runtime.{universe => ru}

object Impls {
  def foo1(c: BlackboxContext) = 2
  def foo2(c: BlackboxContext) = ru.Literal(ru.Constant(42))
}
