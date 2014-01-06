import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    // if optimization didn't run then
    // there should be some useless instructions
    // with the magic constant 3
    val expected = 1
    val got = countMagicThrees(methodNode.instructions)
    assert(got == expected, s"expected $expected but got $got magic threes")
  }

  def countMagicThrees(insnList: InsnList): Int = {
    def isMagicThree(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == asm.Opcodes.ICONST_3)
    insnList.iterator.asScala.count(isMagicThree)
  }
}
