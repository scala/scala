import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    // there should be 1 magic 3 in the lambda body method
    check(1, "Foo_1", "Foo_1$$$anonfun$1")
    // there should be no magic 3 in the apply method
    check(0, "Foo_1$$anonfun$bar$1", "apply")
  }

  def check(expected: Int, className: String, methodName: String) {
    val classNode = loadClassNode(className)
    val methodNode = getMethod(classNode, methodName)
    val got = countMagicThrees(methodNode.instructions)
    assert(got == expected, s"expected $expected but got $got magic threes")    
  }

  def countMagicThrees(insnList: InsnList): Int = {
    def isMagicThree(node: asm.tree.AbstractInsnNode): Boolean =
      (node.getOpcode == asm.Opcodes.ICONST_3)
    insnList.iterator.asScala.count(isMagicThree)
  }
}
