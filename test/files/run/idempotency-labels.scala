import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  val label = reify {
    var x = 0
    while (x < 2) { x += 1 }
    x
  }
  println(label.eval)
  val tb = cm.mkToolBox()
  val tlabel = tb.typecheck(label.tree)
  println(tlabel)
  val rtlabel = tb.untypecheck(tlabel)
  try {
    println(tb.eval(rtlabel))
  } catch {
    case _: ToolBoxError => println("error!")
  }
}