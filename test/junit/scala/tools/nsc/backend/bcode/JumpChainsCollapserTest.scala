package scala.tools.nsc.backend.bcode

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

@RunWith(classOf[JUnit4])
class JumpChainsCollapserTest {

  @Test
  def gotoSimpleInstruction: Unit = {
    val before = {
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
    val transformed = transform(before)
    val fst = transformed.instructions.getFirst
    // the first goto of the multi-hop chain in the input
    // is rephrased to reach the chain's final destination
    // (RETURN in this case) in a single step.
    org.junit.Assert.assertEquals(Opcodes.RETURN, fst.getOpcode)
  }

  @Test
  def gotoLoop: Unit = {
    val before = {
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
    val expected = {
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
    val transformed = transform(before)
    BytecodeAssert.assertEquals(expected, transformed)
  }

  def mkMethodNode = {
    new asm.tree.MethodNode(Opcodes.ACC_PUBLIC, "m", "()V", null, null)
  }

  def transform(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val jcc = new JumpChainsCollapser
    do { jcc.transform(input) } while (jcc.changed)

    input
  }

}
