import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("JC_1")
    // JC_1 source-code contains jumps that EssentialCleanser.
    // removes via scala.tools.asm.optimiz.JumpChainsCollapser
    for (methodNode <- classNode.methods.asScala) {
      val jumps = count(methodNode.instructions, asm.Opcodes.GOTO)
      if (jumps > 7) println(s"Found $jumps GOTO(s) in ${methodNode.name}")
    }
  }

  def count(insnList: InsnList, opcode: Int): Int = {
    def isOpc(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == opcode)
    insnList.iterator.asScala.count(isOpc)
  }
}
