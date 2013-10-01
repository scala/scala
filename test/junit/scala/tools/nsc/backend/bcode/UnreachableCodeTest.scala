package scala.tools.nsc.backend.bcode

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm
import scala.tools.asm.Opcodes
import BytecodeAssert._

@RunWith(classOf[JUnit4])
class UnreachableCodeTest {

  /**
   * Basic test for dead code elimination. Checks whether return instruction that follows
   * throw instruction is removed.
   */
  @Test
  def basicTest: Unit = {
    val before = {
      val m  = mkMethodNode
      m.visitInsn(Opcodes.ACONST_NULL)
      m.visitInsn(Opcodes.ATHROW)
      m.visitInsn(Opcodes.RETURN)
      m
    }
    val after = {
      val m  = mkMethodNode
      m.visitInsn(Opcodes.ACONST_NULL)
      m.visitInsn(Opcodes.ATHROW)
      m
    }
    assertNotEquals(before, after)
    val transformed = transform(before)
    // unreachable code has been removed
    assertEquals(transformed, after)
  }

  /**
   * Basic test for dead code elimination. Checks whether
   * code that is unreachable due to jump instruction is properly removed.
   */
  @Test
  def jumpOverUnreachableCode: Unit = {
    val before = {
      val m  = mkMethodNode
      val L  = new asm.Label
      m.visitJumpInsn(Opcodes.GOTO, L)
      m.visitInsn(Opcodes.ACONST_NULL)
      m.visitInsn(Opcodes.ATHROW)
      m.visitLabel(L)
      m.visitInsn(Opcodes.RETURN)

      m
    }
    val after = {
      val m  = mkMethodNode
      val L  = new asm.Label
      m.visitJumpInsn(Opcodes.GOTO, L)
      m.visitLabel(L)
      m.visitInsn(Opcodes.RETURN)

      m
    }
    assertNotEquals(before, after)
    val transformed = transform(before)
    // unreachable code has been removed
    assertEquals(transformed, after)
  }

  private def mkMethodNode = {
    new asm.tree.MethodNode(Opcodes.ACC_PUBLIC, "m", "()V", null, null)
  }

  private def transform(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val tr = new UnreachableCode
    Util.computeMaxLocalsMaxStack(input)
    do { tr.transform("C", input) } while (tr.changed)

    input
  }

}
