import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val toolbox = cm.mkToolBox()
  val rupkg = cm.staticModule("scala.reflect.runtime.package")
  val rusym = build.selectTerm(rupkg, "universe")
  val NullaryMethodType(rutpe) = rusym.typeSignature
  val ru = build.newFreeTerm("ru", rutpe, scala.reflect.runtime.universe)

  val tree1 = Apply(Select(Ident(ru), newTermName("reify")), List(Apply(Select(Ident(newTermName("scala")), newTermName("Array")), List(Literal(Constant(2))))))
  val ttree1 = toolbox.typeCheck(tree1, withMacrosDisabled = false)
  println(ttree1)

  val tree2 = Apply(Select(Ident(ru), newTermName("reify")), List(Apply(Select(Ident(newTermName("scala")), newTermName("Array")), List(Literal(Constant(2))))))
  val ttree2 = toolbox.typeCheck(tree2, withMacrosDisabled = true)
  println(ttree2)
}
