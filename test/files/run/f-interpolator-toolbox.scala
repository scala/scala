import scala.reflect.runtime.{currentMirror, universe}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.testkit.AssertUtil.assertThrows
import universe._

// the trees have no positions
object Test extends App {
  val tb = currentMirror.mkToolBox()

  assert(tb.eval(q"""val x = 1; f"a $$x%d" """) == "a 1")
  assert(tb.eval(q"""val x = 1; StringContext("a\t", "%2d").f(x)""") == "a\t 1")

  assertThrows[ToolBoxError](tb.eval(q"""val x = 1; f"a $$x%-d" """), _.contains("Flag '-' requires a width"))
}
