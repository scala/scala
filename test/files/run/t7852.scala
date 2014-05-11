import scala.tools.partest.BytecodeTest
import scala.tools.asm
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  val nullChecks = Set(asm.Opcodes.IFNONNULL, asm.Opcodes.IFNULL)

  def show: Unit = {
    def test(methodName: String, expected: Int) {
      val classNode = loadClassNode("Lean")
      val methodNode = getMethod(classNode, methodName)
      val got = countNullChecks(methodNode.instructions)
      assert(got == expected, s"$methodName: expected $expected but got $got comparisons")
    }
    test("string", expected = 0)
    test("module", expected = 0)
    test("moduleIndirect", expected = 2)
  }

  def countNullChecks(insnList: asm.tree.InsnList): Int =
    insnList.iterator.asScala.map(_.getOpcode).count(nullChecks)
}

class Lean {
  def string {
    "" == toString
  }

  def module {
    Nil == (toString: Any)
  }

  def moduleIndirect {
    val n: Nil.type = null
    n == (toString: Any) // still need null checks here.
  }
}
