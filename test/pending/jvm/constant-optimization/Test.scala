
import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  val comparisons = Set(asm.Opcodes.IF_ACMPEQ, asm.Opcodes.IF_ACMPNE, asm.Opcodes.IF_ICMPEQ, asm.Opcodes.IF_ICMPGE, asm.Opcodes.IF_ICMPGT, asm.Opcodes.IF_ICMPLE,
    asm.Opcodes.IF_ICMPLT, asm.Opcodes.IF_ICMPNE, asm.Opcodes.IFEQ, asm.Opcodes.IFGE, asm.Opcodes.IFGT, asm.Opcodes.IFLE, asm.Opcodes.IFLT,
    asm.Opcodes.IFNE, asm.Opcodes.IFNONNULL, asm.Opcodes.IFNULL)

  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    // after optimization there should be no comparisons left
    val expected = 0

    val got = countComparisons(methodNode.instructions)
    assert(got == expected, s"expected $expected but got $got comparisons")
  }

  def countComparisons(insnList: InsnList): Int = {
    def isComparison(node: asm.tree.AbstractInsnNode): Boolean =
      (comparisons contains node.getOpcode)
    insnList.iterator.asScala count isComparison
  }
}