import scala.language.experimental.macros
import scala.reflect.macros.Context

object PartestUtil {
  /**
   * `trace("".isEmpty)` will return `true` and as a side effect print the following to standard out.
   * {{{
   *   trace> "".isEmpty
   *   res: Boolean = true
   *
   * }}}
   *
   * An alternative to [[scala.tools.partest.ReplTest]] that avoids the inconvenience of embedding
   * test code in a string.
   */
  def trace[A](a: A) = macro traceImpl[A]

  def traceImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    import definitions._

    c.Expr(Block(
      List(Apply(
        Select(Ident(PredefModule), TermName("println")),
        List(Apply(
          Select(Apply(
            Select(Ident(ScalaPackage), TermName("StringContext")),
            List(
              Literal(Constant("trace> ")),
              Literal(Constant("\\nres: ")),
              Literal(Constant(" = ")),
              Literal(Constant("\\n")))),
          TermName("s")),
          List(
            Literal(Constant(show(a.tree))),
            Literal(Constant(show(a.actualType))),
            a.tree))))),
      a.tree))
  }
}
