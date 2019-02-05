import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def impl[T](c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
  def foo[T]: Unit = macro impl[T]
}
