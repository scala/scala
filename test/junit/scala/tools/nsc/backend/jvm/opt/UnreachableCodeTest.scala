package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._

@RunWith(classOf[JUnit4])
class UnreachableCodeTest {
  import UnreachableCodeTest._

  @Test
  def basicElimination(): Unit = {
    assertEliminateDead(
      Op(ACONST_NULL),
      Op(ATHROW),
      Op(RETURN).dead
    )

    assertEliminateDead(
      Op(RETURN)
    )

    assertEliminateDead(
      Op(RETURN),
      Op(ACONST_NULL).dead,
      Op(ATHROW).dead
    )
  }

  @Test
  def eliminateNop(): Unit = {
    assertEliminateDead(
      // not dead, since visited by data flow analysis. need a different opt to eliminate it.
      Op(NOP),
      Op(RETURN),
      Op(NOP).dead
    )
  }

  @Test
  def eliminateBranchOver(): Unit = {
    assertEliminateDead(
      Jump(GOTO, Label(1)),
      Op(ACONST_NULL).dead,
      Op(ATHROW).dead,
      Label(1),
      Op(RETURN)
    )

    assertEliminateDead(
      Jump(GOTO, Label(1)),
      Label(1),
      Op(RETURN)
    )
  }

  @Test
  def deadLabelsRemain(): Unit = {
    assertEliminateDead(
      Op(RETURN),
      Jump(GOTO, Label(1)).dead,
      // not dead - labels may be referenced from other places in a classfile (eg exceptions table).
      // will need a different opt to get rid of them
      Label(1)
    )
  }

  @Test
  def pushPopNotEliminated(): Unit = {
    assertEliminateDead(
      // not dead, visited by data flow analysis.
      Op(ACONST_NULL),
      Op(POP),
      Op(RETURN)
    )
  }

  @Test
  def nullnessNotConsidered(): Unit = {
    assertEliminateDead(
      Op(ACONST_NULL),
      Jump(IFNULL, Label(1)),
      Op(RETURN), // not dead
      Label(1),
      Op(RETURN)
    )
  }

  @Test
  def metaTest(): Unit = {
    assertEliminateDead() // no instructions

    assertThrows[AssertionError](
      assertEliminateDead(Op(RETURN).dead),
      _.contains("Expected: List()\nActual  : List(Op(RETURN))")
    )

    assertThrows[AssertionError](
      assertEliminateDead(Op(RETURN), Op(RETURN)),
      _.contains("Expected: List(Op(RETURN), Op(RETURN))\nActual  : List(Op(RETURN))")
    )
  }

  @Test
  def bytecodeEquivalence: Unit = {
    assertTrue(List(VarOp(ILOAD, 1)) ===
               List(VarOp(ILOAD, 2)))
    assertTrue(List(VarOp(ILOAD, 1), VarOp(ISTORE, 1)) ===
               List(VarOp(ILOAD, 2), VarOp(ISTORE, 2)))

    // the first Op will associate 1->2, then the 2->2 will fail
    assertFalse(List(VarOp(ILOAD, 1), VarOp(ISTORE, 2)) ===
                List(VarOp(ILOAD, 2), VarOp(ISTORE, 2)))

    // will associate 1->2 and 2->1, which is OK
    assertTrue(List(VarOp(ILOAD, 1), VarOp(ISTORE, 2)) ===
               List(VarOp(ILOAD, 2), VarOp(ISTORE, 1)))

    assertTrue(List(Label(1), Label(2), Label(1)) ===
               List(Label(2), Label(4), Label(2)))
    assertTrue(List(LineNumber(1, Label(1)), Label(1)) ===
               List(LineNumber(1, Label(3)), Label(3)))
    assertFalse(List(LineNumber(1, Label(1)), Label(1)) ===
                List(LineNumber(1, Label(3)), Label(1)))

    assertTrue(List(TableSwitch(TABLESWITCH, 1, 3, Label(4), List(Label(5), Label(6))), Label(4), Label(5), Label(6)) ===
               List(TableSwitch(TABLESWITCH, 1, 3, Label(9), List(Label(3), Label(4))), Label(9), Label(3), Label(4)))

    assertTrue(List(FrameEntry(F_FULL, List(INTEGER, DOUBLE, Label(3)), List("java/lang/Object", Label(4))), Label(3), Label(4)) ===
               List(FrameEntry(F_FULL, List(INTEGER, DOUBLE, Label(1)), List("java/lang/Object", Label(3))), Label(1), Label(3)))
  }
}

object UnreachableCodeTest {
  import scala.language.implicitConversions
  implicit def aliveInstruction(ins: Instruction): (Instruction, Boolean) = (ins, true)

  implicit class MortalInstruction(val ins: Instruction) extends AnyVal {
    def dead: (Instruction, Boolean) = (ins, false)
  }

  def assertEliminateDead(code: (Instruction, Boolean)*): Unit = {
    val cls = wrapInClass(genMethod()(code.map(_._1): _*))
    LocalOpt.removeUnreachableCode(cls)
    val nonEliminated = instructionsFromMethod(cls.methods.get(0))

    val expectedLive = code.filter(_._2).map(_._1).toList

    assertTrue(s"\nExpected: $expectedLive\nActual  : $nonEliminated", nonEliminated === expectedLive)
  }

}
