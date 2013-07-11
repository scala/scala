import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = cm.mkToolBox()
  val tree = Select(This(cm.staticPackage("scala").moduleClass), TermName("Predef"))
  val ttree = tb.typeCheck(tree)
  val rttree = tb.resetAllAttrs(ttree)
  println(tb.eval(rttree) == Predef)
}
