import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import internal._

// Note: If you're looking at this test and you don't know why, you may
// have accidentally changed the way type tags reify.  If so, validate
// that your changes are accurate and update the check file.

object Test extends App {
  val toolbox = cm.mkToolBox()
  val rupkg = cm.staticModule("scala.reflect.runtime.package")
  val rusym = reificationSupport.selectTerm(rupkg, "universe")
  val NullaryMethodType(rutpe) = rusym.info
  val ru = reificationSupport.newFreeTerm("ru", scala.reflect.runtime.universe)
  reificationSupport.setInfo(ru, rutpe)

  val tree1 = Apply(Select(Ident(ru), TermName("reify")), List(Literal(Constant(2))))
  val ttree1 = toolbox.typecheck(tree1, withMacrosDisabled = false)
  println(ttree1)

  val tree2 = Apply(Select(Ident(ru), TermName("reify")), List(Literal(Constant(2))))
  val ttree2 = toolbox.typecheck(tree2, withMacrosDisabled = true)
  println(ttree2)
}
