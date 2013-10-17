import scala.reflect.macros.Context

object Macros {
  def impl_with_implicits_enabled(c: Context) = {
    import c.universe._

    val tree1 = Apply(Select(Literal(Constant(1)), TermName("$minus$greater")), List(Literal(Constant(2))))
    val ttree1 = c.typeCheck(tree1, withImplicitViewsDisabled = false)
    c.Expr[String](Literal(Constant(ttree1.toString)))
  }

  def foo_with_implicits_enabled = macro impl_with_implicits_enabled

  def impl_with_implicits_disabled(c: Context) = {
    import c.universe._

    try {
      val tree2 = Apply(Select(Literal(Constant(1)), TermName("$minus$greater")), List(Literal(Constant(2))))
      val ttree2 = c.typeCheck(tree2, withImplicitViewsDisabled = true)
      c.Expr[String](Literal(Constant(ttree2.toString)))
    } catch {
      case ex: Throwable =>
        c.Expr[String](Literal(Constant(ex.toString)))
    }
  }

  def foo_with_implicits_disabled = macro impl_with_implicits_disabled
}