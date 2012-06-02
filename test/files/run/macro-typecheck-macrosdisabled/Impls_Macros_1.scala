import scala.reflect.makro.Context

object Macros {
  def impl_with_macros_enabled(c: Context) = {
    import c.universe._

    val ru = Select(Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")), newTermName("runtime")), newTermName("package")), newTermName("universe"))
    val tree1 = Apply(Select(ru, newTermName("reify")), List(Literal(Constant(2))))
    val ttree1 = c.typeCheck(tree1, withMacrosDisabled = false)
    c.literal(ttree1.toString)
  }

  def foo_with_macros_enabled = macro impl_with_macros_enabled

  def impl_with_macros_disabled(c: Context) = {
    import c.universe._

    val rupkg = c.mirror.staticModule("scala.reflect.runtime.package")
    val rusym = build.selectTerm(rupkg, "universe")
    val NullaryMethodType(rutpe) = rusym.typeSignature
    val ru = build.newFreeTerm("ru", rutpe, scala.reflect.runtime.universe)

    val tree2 = Apply(Select(Ident(ru), newTermName("reify")), List(Literal(Constant(2))))
    val ttree2 = c.typeCheck(tree2, withMacrosDisabled = true)
    c.literal(ttree2.toString)
  }

  def foo_with_macros_disabled = macro impl_with_macros_disabled
}