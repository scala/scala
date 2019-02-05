import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def impl[T, U](c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T, U]: Unit = macro impl[T, U]
}
