import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    assert(countNops(methodNode.instructions) == 0)
  }

  def countNops(insnList: InsnList): Int = {
    def isNop(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == asm.Opcodes.NOP)
    insnList.iterator.asScala.count(isNop)
  }
}
