import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo[U <: String](c: Ctx) = { import c.universe._; c.Expr[Unit](q"()") }
}
