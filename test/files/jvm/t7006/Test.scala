import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    assert(count(methodNode.instructions, asm.Opcodes.NOP) == 0)
    assert(count(methodNode.instructions, asm.Opcodes.GOTO) == 1)
  }

  def count(insnList: InsnList, opcode: Int): Int = {
    def isNop(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == opcode)
    insnList.iterator.asScala.count(isNop)
  }
}
