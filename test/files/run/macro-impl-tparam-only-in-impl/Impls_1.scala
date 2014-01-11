import scala.reflect.macros.blackbox.Context

object Impls {
  def foo[U <: String](c: Context): c.Expr[Unit] = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
}
