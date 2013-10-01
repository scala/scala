package scala.tools.nsc.backend.bcode

import org.junit.Test
import org.junit.Assert
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm
import scala.tools.asm.Opcodes

import BytecodeAssert._

@RunWith(classOf[JUnit4])
class LabelsCleanupTest {

  @Test
  def removeLineNumberNodes: Unit = {
    val before = {
      val m  = mkMethodNode
      val L = new asm.Label
      //     return
      m.visitInsn(Opcodes.RETURN)
      // line number 1
      m.visitLineNumber(1, L)
      // line number 2
      m.visitLineNumber(2, L)
      // L:
      m.visitLabel(L)

      m
    }
    val expected = {
      val m  = mkMethodNode
      //     return
      m.visitInsn(Opcodes.RETURN)

      m
    }
    assertNotEquals(before, expected)
    val transformed  = transform(before)
    // redundant LineNumberNodes are removed
    assertEquals(transformed, expected)
  }

  @Test
  def removeLabelNodes: Unit = {
    val before: asm.tree.MethodNode = {
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
    val expected: asm.tree.MethodNode = {
      val m  = mkMethodNode
      //     return
      m.visitInsn(Opcodes.RETURN)
      m
    }
    assertNotEquals(before, expected)
    val transformed  = transform(before)
    // redundant LabelNodes are removed
    assertEquals(transformed, expected)
  }

  @Test
  def removeLocalVariables: Unit = {
    val before: asm.tree.MethodNode = {
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
    val expected: asm.tree.MethodNode = {
      val m  = mkMethodNode
      //     nop
      m.visitInsn(Opcodes.NOP)
      //     return
      m.visitInsn(Opcodes.RETURN)

      m
    }
    assertNotEquals(before, expected)
    val transformed  = transform(before)
    // redundant LocalVariableEntry's are removed
    Assert.assertTrue(transformed.localVariables.size() == 0)
    assertEquals(transformed, expected)
  }

  private def mkMethodNode = {
    new asm.tree.MethodNode(Opcodes.ACC_PUBLIC, "m", "()V", null, null)
  }

  private def transform(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val tr = new LabelsCleanup
    do { tr.transform(input) } while (tr.changed)

    input
  }

}
