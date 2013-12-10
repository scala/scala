import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._
object O { def apply() = 0 }
val ORef = reify { O }.tree
val tree = Apply(Block(Nil, Block(Nil, ORef)), Nil)
{val tb = reflect.runtime.currentMirror.mkToolBox(); tb.typecheck(tree): Any}
"""
}
