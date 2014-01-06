import scala.reflect.macros.BlackboxContext

object Macros {
  def impl[T](c: BlackboxContext) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T] = macro impl[T]
}