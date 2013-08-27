import scala.tools.nsc.backend.bcode.LabelsCleanup
import scala.tools.partest.BytecodeTest
import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

object Test extends BytecodeTest {

  def show: Unit = {
    val ma  = transformed(before())
    val mb  = after()
    val isa = wrapped(ma)
    val isb = wrapped(mb)
    // redundant LabelNodes are removed
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
    // L1:
    m.visitLabel(L1)
    // L2:
    m.visitLabel(L2)
    //     return
    m.visitInsn(Opcodes.RETURN)

    m
  }

  def after(): asm.tree.MethodNode = {
    val m  = mkMethodNode
    //     return
    m.visitInsn(Opcodes.RETURN)

    m
  }

  def transformed(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val tr = new LabelsCleanup
    do { tr.transform(input) } while (tr.changed)

    input
  }

}
