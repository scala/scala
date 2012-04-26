import scala.reflect.mirror._

object Test extends App {
  val toolbox = mkToolBox()
  val mrPkg = staticModule("scala.reflect.package")
  val mrSym = selectTerm(mrPkg, "mirror")
  val NullaryMethodType(mrTpe) = mrSym.typeSignature
  val mr = newFreeTerm("mr", mrTpe, scala.reflect.mirror)

  val tree1 = Apply(Select(Ident(mr), newTermName("reify")), List(Literal(Constant(2))))
  val ttree1 = toolbox.typeCheck(tree1, withMacrosDisabled = false)
  println(ttree1)

  val tree2 = Apply(Select(Ident(mr), newTermName("reify")), List(Literal(Constant(2))))
  val ttree2 = toolbox.typeCheck(tree2, withMacrosDisabled = true)
  println(ttree2)
}
