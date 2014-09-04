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

  // jvm-1.6 enables emitting stack map frames, which impacts the code generation wrt dead basic blocks,
  // see comment in BCodeBodyBuilder
  val dceCompiler = newCompiler(extraArgs = "-target:jvm-1.6 -Ybackend:GenBCode -Yopt:unreachable-code")
  val noOptCompiler = newCompiler(extraArgs = "-target:jvm-1.6 -Ybackend:GenBCode -Yopt:l:none")

  // jvm-1.5 disables computing stack map frames, and it emits dead code as-is.
  val noOptNoFramesCompiler = newCompiler(extraArgs = "-target:jvm-1.5 -Ybackend:GenBCode -Yopt:l:none")

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
  def basicEliminationCompiler(): Unit = {
    val code = "def f: Int = { return 1; 2 }"
    val withDce = singleMethodInstructions(dceCompiler)(code)
    assertSameCode(withDce.dropNonOp, List(Op(ICONST_1), Op(IRETURN)))

    val noDce = singleMethodInstructions(noOptCompiler)(code)

    // The emitted code is ICONST_1, IRETURN, ICONST_2, IRETURN. The latter two are dead.
    //
    // GenBCode puts the last IRETURN into a new basic block: it emits a label before the second
    // IRETURN. This is an implementation detail, it may change; it affects the outcome of this test.
    //
    // During classfile writing with COMPUTE_FAMES (-target:jvm-1.6 or larger), the ClassfileWriter
    // puts the ICONST_2 into a new basic block, because the preceding operation (IRETURN) ends
    // the current block. We get something like
    //
    //   L1: ICONST_1; IRETURN
    //   L2: ICONST_2            << dead
    //   L3: IRETURN             << dead
    //
    // Finally, instructions in the dead basic blocks are replaced by ATHROW, as explained in
    // a comment in BCodeBodyBuilder.
    assertSameCode(noDce.dropNonOp, List(Op(ICONST_1), Op(IRETURN), Op(ATHROW), Op(ATHROW)))

    // when NOT computing stack map frames, ASM's ClassWriter does not replace dead code by NOP/ATHROW
    val noDceNoFrames = singleMethodInstructions(noOptNoFramesCompiler)(code)
    assertSameCode(noDceNoFrames.dropNonOp, List(Op(ICONST_1), Op(IRETURN), Op(ICONST_2), Op(IRETURN)))
  }

  @Test
  def eliminateDeadCatchBlocks(): Unit = {
    val code = "def f: Int = { return 0; try { 1 } catch { case _: Exception => 2 } }"
    assertSameCode(singleMethodInstructions(dceCompiler)(code).dropNonOp,
                   List(Op(ICONST_0), Op(IRETURN)))

    val code2 = "def f: Unit = { try { } catch { case _: Exception => () }; () }"
    // DCE only removes dead basic blocks, but not NOPs, and also not useless jumps
    assertSameCode(singleMethodInstructions(dceCompiler)(code2).dropNonOp,
                   List(Op(NOP), Jump(GOTO, Label(33)), Label(33), Op(RETURN)))

    val code3 = "def f: Unit = { try { } catch { case _: Exception => try { } catch { case _: Exception => () } }; () }"
    assertSameCode(singleMethodInstructions(dceCompiler)(code3).dropNonOp,
      List(Op(NOP), Jump(GOTO, Label(33)), Label(33), Op(RETURN)))

    val code4 = "def f: Unit = { try { try { } catch { case _: Exception => () } } catch { case _: Exception => () }; () }"
    assertSameCode(singleMethodInstructions(dceCompiler)(code4).dropNonOp,
      List(Op(NOP), Jump(GOTO, Label(4)), Label(4), Jump(GOTO, Label(7)), Label(7), Op(RETURN)))
  }

  @Test // test the dce-testing tools
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
    assertSameCode(nonEliminated, expectedLive)
  }
}
