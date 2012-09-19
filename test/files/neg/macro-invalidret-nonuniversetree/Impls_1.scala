import scala.reflect.macros.{Context => Ctx}
import scala.reflect.runtime.{universe => ru}

object Impls {
  def foo(c: Ctx) = ru.Literal(ru.Constant(42))
}
