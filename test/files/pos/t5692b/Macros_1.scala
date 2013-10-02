import scala.reflect.macros.BlackboxContext

object Macros {
  def impl[T, U](c: BlackboxContext) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T, U] = macro impl[T, U]
}