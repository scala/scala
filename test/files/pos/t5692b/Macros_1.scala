import scala.reflect.macros.Context

object Macros {
  def impl[T, U](c: Context) = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
  def foo[T, U] = macro impl[T, U]
}