import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

// Related to SI-6187
//
// Moved to pending as we are currently blocked by the inability
// to reify the parent types of the anonymous function class,
// which are not part of the tree, but rather only part of the
// ClassInfoType.
object Test extends App {
  val partials = reify {
    List((false,true)) collect { case (x,true) => x }
  }
  println(Seq(show(partials), showRaw(partials)).mkString("\n\n"))
  try {
    println(partials.eval)
  } catch {
    case e: ToolBoxError => println(e)
  }
  val tb = cm.mkToolBox()
  val tpartials = tb.typecheck(partials.tree)
  println(tpartials)
  val rtpartials = tb.untypecheck(tpartials)
  println(tb.eval(rtpartials))
}
Test.main(null)