import scala.tools.nsc.backend.bcode.JumpChainsCollapser
import scala.tools.partest.BytecodeTest
import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

object Test extends BytecodeTest {

  def show: Unit = {
    val fst = transformed(before()).instructions.getFirst
    // the first goto of the multi-hop chain in the input
    // is rephrased to reach the chain's final destination
    // (RETURN in this case) in a single step.
    assert(fst.getOpcode == Opcodes.RETURN)
  }

  def mkMethodNode = {
    new asm.tree.MethodNode(
      Opcodes.ACC_PUBLIC,
      "m",
      "()V",
      null, null
    )
  }

  def before(): asm.tree.MethodNode = {
    val m  = mkMethodNode
    val L1 = new asm.Label
    val L2 = new asm.Label
    //     goto L1
    m.visitJumpInsn(Opcodes.GOTO, L1)
    // L1: goto L2
    m.visitLabel(L1)
    m.visitJumpInsn(Opcodes.GOTO, L2)
    // L2: return
    m.visitLabel(L2)
    m.visitInsn(Opcodes.RETURN)

    m
  }

  def transformed(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val jcc = new JumpChainsCollapser
    do { jcc.transform(input) } while (jcc.changed)

    input
  }

}
