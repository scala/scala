import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U <: String](c: Ctx) = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
}
