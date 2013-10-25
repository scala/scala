import scala.reflect.macros.Context

object Macros {
  def impl[T](c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T] = macro impl[T]
}