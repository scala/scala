import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U <: String](c: Ctx): c.Expr[Unit] = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
}
