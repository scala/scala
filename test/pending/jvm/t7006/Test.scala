import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    val nopCount  = count(methodNode.instructions, asm.Opcodes.NOP)
    val gotoCount = count(methodNode.instructions, asm.Opcodes.GOTO)
    assert(nopCount  == 0, s"NOPs  expected: 0, actual: $nopCount")
    assert(gotoCount == 1, s"GOTOs expected: 1, actual: $gotoCount")
  }

  def count(insnList: InsnList, opcode: Int): Int = {
    def isNop(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == opcode)
    insnList.iterator.asScala.count(isNop)
  }
}
