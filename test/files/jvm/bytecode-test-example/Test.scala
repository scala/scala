import scala.tools.partest.BytecodeTest

import scala.tools.nsc.util.JavaClassPath
import java.io.InputStream
import scala.tools.asm
import asm.ClassReader
import asm.tree.{ClassNode, InsnList}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    println(countNullChecks(methodNode.instructions))
  }

  def countNullChecks(insnList: InsnList): Int = {
    /** Is given instruction a null check?
     *  NOTE
     *   This will detect direct null comparison as in
     *    if (x == null) ...
     *   and not indirect as in
     *     val foo = null
     *     if (x == foo) ...
     */
    def isNullCheck(node: asm.tree.AbstractInsnNode): Boolean = {
      val opcode = node.getOpcode
      (opcode == asm.Opcodes.IFNULL) || (opcode == asm.Opcodes.IFNONNULL)
    }
    insnList.iterator.asScala.count(isNullCheck)
  }
}
