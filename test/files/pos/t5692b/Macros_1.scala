import scala.reflect.macros.blackbox.Context

object Macros {
  def impl[T, U](c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T, U] = macro impl[T, U]
}