import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  val casee = reify {
    case class C(x: Int, y: Int)
    println(C(2, 3))
  }
  println(casee.eval)
  val tb = cm.mkToolBox()
  val tcasee = tb.typecheck(casee.tree)
  println(tcasee)
  val rtcasee = tb.untypecheck(tcasee)
  try {
    println(tb.eval(rtcasee))
  } catch {
    // this is the current behaviour, rather than the desired behavior; see SI-5467
    case _: ToolBoxError => println("error!")
  }
}