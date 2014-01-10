import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  val thiss = reify {
    List[String]("")
  }
  println(thiss.eval)
  val tb = cm.mkToolBox()
  val tthiss = tb.typecheck(thiss.tree)
  println(tthiss)
  println(showRaw(tthiss))
  val rtthiss = tb.untypecheck(tthiss)
  try {
    println(tb.eval(rtthiss))
  } catch {
    // this is the current behaviour, rather than the desired behavior; see SI-5705
    case _: ToolBoxError => println("error!")
  }
}