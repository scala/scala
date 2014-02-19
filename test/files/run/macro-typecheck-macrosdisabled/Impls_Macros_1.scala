import scala.reflect.macros.blackbox.Context

object Macros {
  def impl_with_macros_enabled(c: Context) = {
    import c.universe._

    val ru = Select(Select(Select(Select(Ident(TermName("scala")), TermName("reflect")), TermName("runtime")), TermName("package")), TermName("universe"))
    val tree1 = Apply(Select(ru, TermName("reify")), List(Literal(Constant(2))))
    val ttree1 = c.typecheck(tree1, withMacrosDisabled = false)
    c.Expr[String](Literal(Constant(ttree1.toString)))
  }

  def foo_with_macros_enabled = macro impl_with_macros_enabled

  def impl_with_macros_disabled(c: Context) = {
    import c.universe._
    import internal._

    val rupkg = c.mirror.staticModule("scala.reflect.runtime.package")
    val rusym = reificationSupport.selectTerm(rupkg, "universe")
    val NullaryMethodType(rutpe) = rusym.info
    val ru = reificationSupport.newFreeTerm("ru", scala.reflect.runtime.universe)
    reificationSupport.setInfo(ru, rutpe)

    val tree2 = Apply(Select(Ident(ru), TermName("reify")), List(Literal(Constant(2))))
    val ttree2 = c.typecheck(tree2, withMacrosDisabled = true)
    c.Expr[String](Literal(Constant(ttree2.toString)))
  }

  def foo_with_macros_disabled = macro impl_with_macros_disabled
}