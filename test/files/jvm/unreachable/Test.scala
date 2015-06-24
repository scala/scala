import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    // Foo_1 is full of unreachable code which if not eliminated
    // will result in NOPs as can be confirmed by adding -Ydisable-unreachable-prevention
    // to Foo_1.flags
    for (methodNode <- classNode.methods.asScala) {
      val got = count(methodNode.instructions, asm.Opcodes.NOP)
      if (got != 0) println(s"Found $got NOP(s) in ${methodNode.name}")
    }
  }

  def count(insnList: InsnList, opcode: Int): Int = {
    def isNop(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == opcode)
    insnList.iterator.asScala.count(isNop)
  }
}