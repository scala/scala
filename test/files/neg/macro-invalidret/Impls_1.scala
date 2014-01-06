import scala.reflect.macros.BlackboxContext
import scala.reflect.runtime.{universe => ru}

object Impls {
  def foo1(c: BlackboxContext) = 2
  def foo2(c: BlackboxContext) = ru.Literal(ru.Constant(42))
  def foo3(c: BlackboxContext) = ???
  def foo5(c: BlackboxContext) = c.universe.Literal(c.universe.Constant(42))
  def foo6(c: BlackboxContext) = c.Expr[Int](c.universe.Literal(c.universe.Constant(42)))
}
