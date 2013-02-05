import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def foo[U: Numeric](x: U) = macro foo_impl[U]

  def foo_impl[U](c: Context)(x: c.Expr[U])(evidence: c.Expr[Numeric[U]]) = {
    import c.universe._
    val plusOne = Apply(Select(evidence.tree, newTermName("plus")), List(x.tree, Literal(Constant(1))))
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(plusOne))
    c.Expr[Unit](body)
  }
}