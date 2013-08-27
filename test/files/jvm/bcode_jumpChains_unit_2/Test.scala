import scala.tools.nsc.backend.bcode.JumpChainsCollapser
import scala.tools.partest.BytecodeTest
import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

object Test extends BytecodeTest {

  def show: Unit = {
    val isa = wrapped(transformed(before()))
    val isb = wrapped(after())
    assert(isa == isb)
  }

  def wrapped(m: asm.tree.MethodNode) = instructions.fromMethod(m)

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
    // L2: goto L2
    m.visitLabel(L2)
    m.visitJumpInsn(Opcodes.GOTO, L2)
    //     return
    m.visitInsn(Opcodes.RETURN)

    m
  }

  def after(): asm.tree.MethodNode = {
    val L1 = new asm.Label
    val L2 = new asm.Label
    val m  = mkMethodNode
    //     goto L2
    m.visitJumpInsn(Opcodes.GOTO, L2)
    // L1: goto L2
    m.visitLabel(L1)
    m.visitJumpInsn(Opcodes.GOTO, L2)
    // L2: goto L2
    m.visitLabel(L2)
    m.visitJumpInsn(Opcodes.GOTO, L2)
    //     return
    m.visitInsn(Opcodes.RETURN)

    m
  }

  def transformed(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val jcc = new JumpChainsCollapser
    do { jcc.transform(input) } while (jcc.changed)

    input
  }

}
