import scala.reflect.macros.blackbox.Context

object Macros {
  def impl[T](c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T] = macro impl[T]
}