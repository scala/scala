import scala.language.existentials
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval
import java.lang.reflect.InvocationTargetException

class O { class I }

class A extends O {
  val x = new O
  val code = reify {
    val v: x.I = ???
    v
  }
  println(showRaw(code))
}

object Test extends App {
  try {
    val v = (new A).code.eval
  } catch {
    case ex: InvocationTargetException if ex.getCause.isInstanceOf[NotImplementedError] =>
  }
}
