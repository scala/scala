import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  val lazee = reify {
    class C {
      lazy val x = 2
      implicit lazy val y = 3
    }
    val c = new C()
    import c._
    x * implicitly[Int]
  }
  println(lazee.eval)
  val tb = cm.mkToolBox()
  val tlazee = tb.typecheck(lazee.tree)
  println(tlazee)
  val rtlazee = tb.untypecheck(tlazee)
  try {
    println(tb.eval(rtlazee))
  } catch {
    // this is the current behaviour, rather than the desired behavior; see SI-5466
    case _: ToolBoxError => println("error!")
  }
}