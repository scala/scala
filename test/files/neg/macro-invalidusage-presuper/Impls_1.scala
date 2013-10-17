import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
}