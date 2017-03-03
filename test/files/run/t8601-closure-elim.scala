import scala.tools.partest.BytecodeTest
import scala.tools.partest.ASMConverters.instructionsFromMethod
import scala.tools.asm
import scala.tools.asm.util._
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  val nullChecks = Set(asm.Opcodes.NEW)

  def show: Unit = {
    def test(methodName: String) {
      val classNode = loadClassNode("Foo")
      val methodNode = getMethod(classNode, "b")
      val instrs = instructionsFromMethod(methodNode)
      val ops = methodNode.instructions.iterator.asScala.map(_.getOpcode).toList
      assert(!ops.contains(asm.Opcodes.NEW), instrs)// should be allocation free if the closure is eliminated
    }
    test("b")
  }
}

class Foo {
  @inline final def a(x: Int => Int) = x(1)
  final def b {
    val delta = 0
    a(x => delta + 1)
  }
}
