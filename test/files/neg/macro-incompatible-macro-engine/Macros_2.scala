import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext) = c.universe.Literal(c.universe.Constant(()))
  def foo: Unit = macro impl
}