import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  object Extractor { def unapply(x: Int): Option[Int] = Some(x) }
  val extractor = reify {
    2 match { case Extractor(x) => x }
  }
  println(extractor.eval)
  val tb = cm.mkToolBox()
  val textractor = tb.typecheck(extractor.tree)
  println(textractor)
  val rtextractor = tb.untypecheck(textractor)
  try {
    println(tb.eval(rtextractor))
  } catch {
    // this is the current behaviour, rather than the desired behavior; see SI-5465
    case _: ToolBoxError => println("error!")
  }
}