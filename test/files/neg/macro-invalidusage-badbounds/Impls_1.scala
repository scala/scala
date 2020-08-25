// scalac: -language:experimental.macros
import scala.reflect.macros.blackbox.Context

object Impls {
  def foo[U <: String](c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
}
