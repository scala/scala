package scala.tools.nsc.backend.bcode

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

@RunWith(classOf[JUnit4])
class DanglingExcHandlersUnit1Test {

  @Test
  def show: Unit = {
    val b   = before()
    assert(b.tryCatchBlocks.size() > 0)
    val t   = transformed(b)
    val isa = wrapped(b)
    val isb = wrapped(t)
    // redundant exception-entry has been removed
    assert(t.tryCatchBlocks.size() == 0)
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
    val L3 = new asm.Label
    // L1:
    m.visitLabel(L1)
    //     nop
    m.visitInsn(Opcodes.NOP)
    // L2: --- normal control-flow
    m.visitLabel(L2)
    //     return
    m.visitInsn(Opcodes.RETURN)
    // L3: --- exceptional control-flow
    m.visitLabel(L3)
    //     return
    m.visitInsn(Opcodes.RETURN)

    m.visitTryCatchBlock(L1, L2, L3, null)

    m
  }

  def transformed(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val tr = new DanglingExcHandlers
    do { tr.transform(input) } while (tr.changed)

    input
  }

}
