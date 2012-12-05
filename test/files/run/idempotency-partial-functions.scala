import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  val partials = reify {
    List((false,true)) collect { case (x,true) => x }
  }
  try {
    println(partials.eval)
  } catch {
    case _: ToolBoxError => println("error!!")
  }
  try {
    val tb = cm.mkToolBox()
    val tpartials = tb.typeCheck(partials.tree)
    println(tpartials)
    val rtpartials = tb.resetAllAttrs(tpartials)
    println(tb.eval(rtpartials))
  } catch {
    // this is the current behaviour, rather than the desired behavior; see SI-6187
    case _: ToolBoxError => println("error!")
  }
}