
import scala.language.dynamics
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class C extends Dynamic {
  def selectDynamic(s: String) = 42
}

object Test extends App {
  val c = new C
  assert(c.foo == 42)

  val toolbox = currentMirror.mkToolBox()
  toolbox.typecheck(q"class C extends Dynamic { def selectDynamic(s: String) = ??? }; val c = new C; c.foo")
  toolbox.typecheck(q"42 match { case i => 27 }") // check rewrite to case i @ _
}
