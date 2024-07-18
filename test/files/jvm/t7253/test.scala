//> using options -Werror -Xlint

import scala.tools.asm.Opcodes
import scala.tools.partest.BytecodeTest
import scala.tools.testkit.ASMConverters._

object Test extends BytecodeTest {

  def show(): Unit = {
    def fooOf(name: String) = instructionsFromMethod(getMethod(loadClassNode(name), "foo")).filter(isInvoke)
    cmpInstructions(fooOf("ScalaClient_1"), fooOf("JavaClient_1"))
  }

  def cmpInstructions(isa: List[Instruction], isb: List[Instruction]) =
    if (!isa.sameElements(isb))
      diffInstructions(isa, isb)

  def isInvoke(node: Instruction): Boolean =
    node.opcode == Opcodes.INVOKEVIRTUAL || node.opcode == Opcodes.INVOKEINTERFACE
}
