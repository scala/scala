package scala.tools.nsc.backend.bcode

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

@RunWith(classOf[JUnit4])
class LabelsCleanupUnit3Test {

  @Test
  def show: Unit = {
    val ma  = transformed(before())
    val mb  = after()
    val isa = wrapped(ma)
    val isb = wrapped(mb)
    // redundant LocalVariableEntry's are removed
    assert(ma.localVariables.size() == 0)
    assert(isa == isb)
  }

  def wrapped(m: asm.tree.MethodNode) = {
    Util.computeMaxLocalsMaxStack(m)
    Util.textify(m)
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
    // L1:
    m.visitLabel(L1)
    //     nop
    m.visitInsn(Opcodes.NOP)
    // L2:
    m.visitLabel(L2)
    //     return
    m.visitInsn(Opcodes.RETURN)

    m.visitLocalVariable("abc", "I", null, L1, L2, 1)

    m
  }

  def after(): asm.tree.MethodNode = {
    val m  = mkMethodNode
    //     nop
    m.visitInsn(Opcodes.NOP)
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
