import scala.reflect.macros.BlackboxContext

object Impls {
  def impl(c: BlackboxContext) = { import c.universe._; c.Expr[Unit](q"()") }
}