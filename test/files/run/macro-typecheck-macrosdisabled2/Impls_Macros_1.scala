import scala.reflect.makro.Context

object Macros {
  def impl_with_macros_enabled(c: Context) = {
    import c.mirror._

    val mr = Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")), newTermName("package")), newTermName("mirror"))
    val tree1 = Apply(Select(mr, newTermName("reify")), List(Apply(Select(Ident(newTermName("scala")), newTermName("Array")), List(Literal(Constant(2))))))
    val ttree1 = c.typeCheck(tree1, withMacrosDisabled = false)
    c.literal(ttree1.toString)
  }

  def foo_with_macros_enabled = macro impl_with_macros_enabled

  def impl_with_macros_disabled(c: Context) = {
    import c.mirror._

    val mrPkg = staticModule("scala.reflect.package")
    val mrSym = selectTerm(mrPkg, "mirror")
    val NullaryMethodType(mrTpe) = mrSym.typeSignature
    val mr = newFreeTerm("mr", mrTpe, scala.reflect.mirror)

    val tree2 = Apply(Select(Ident(mr), newTermName("reify")), List(Apply(Select(Ident(newTermName("scala")), newTermName("Array")), List(Literal(Constant(2))))))
    val ttree2 = c.typeCheck(tree2, withMacrosDisabled = true)
    c.literal(ttree2.toString)
  }

  def foo_with_macros_disabled = macro impl_with_macros_disabled
}