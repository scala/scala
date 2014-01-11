import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def foo[U: Numeric](x: U) = macro foo_impl[U]
  def bar[U: Numeric : Equiv, Y <% String](x: U)(implicit s: String) = macro bar_impl[U, Y]

  def foo_impl[U](c: Context)(x: c.Expr[U])(numeric: c.Expr[Numeric[U]]) = {
    import c.universe._
    val plusOne = Apply(Select(numeric.tree, newTermName("plus")), List(x.tree, Literal(Constant(1))))
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(plusOne))
    c.Expr[Unit](body)
  }

  def bar_impl[U, Y](c: Context)(x: c.Expr[U])(numeric: c.Expr[Numeric[U]], equiv: c.Expr[Equiv[U]], viewAsString: c.Expr[Y => String], s: c.Expr[String]) = {
    import c.universe._
    val plusOne = Apply(Select(numeric.tree, newTermName("plus")), List(x.tree, Literal(Constant(1))))
    val plusLen = Apply(Select(numeric.tree, newTermName("plus")), List(plusOne, Select(s.tree, newTermName("length"))))
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(plusLen))
    c.Expr[Unit](body)
  }
}