import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

class O { class I }

object A extends O {
  val code = reify {
    val v: I = new I
    v
  }
  println(showRaw(code.tree))
}

object Test extends App {
  val v: A.I = A.code.eval
}
