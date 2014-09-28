import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}

object Impls {
  def foo1(c: Context) = 2
  def foo2(c: Context) = ru.Literal(ru.Constant(42))
  def foo3(c: Context) = throw null
  def foo5(c: Context) = c.universe.Literal(c.universe.Constant(42))
  def foo6(c: Context) = c.Expr[Int](c.universe.Literal(c.universe.Constant(42)))
}
