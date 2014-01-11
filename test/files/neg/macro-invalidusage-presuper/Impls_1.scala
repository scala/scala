import scala.reflect.macros.blackbox.Context

object Impls {
  def impl(c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
}