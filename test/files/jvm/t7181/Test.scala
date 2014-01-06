import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    // there should be 2 copies of the finally block, each with the magic constant 3
    // one for the "normal" exit
    // one for the uncaught exception exit
    // prior to this PR there would have been 4 since each exception handler would also get a copy
    val expected = 2
    val got = countMagicThrees(methodNode.instructions)
    assert(got == expected, s"expected $expected but got $got magic threes")
  }

  def countMagicThrees(insnList: InsnList): Int = {
    def isMagicThree(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == asm.Opcodes.ICONST_3)
    insnList.iterator.asScala.count(isMagicThree)
  }
}
