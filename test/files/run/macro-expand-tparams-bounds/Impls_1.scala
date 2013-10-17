import scala.reflect.macros.Context

object Impls1 {
  def foo[U <: String](c: Context): c.Expr[Unit] = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
}

class C
class D extends C

object Impls2 {
  def foo[U <: C](c: Context): c.Expr[Unit] = { import c.universe._; c.Expr[Unit](Literal(Constant(()))) }
}
