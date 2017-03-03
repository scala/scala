package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.AssertUtil._
import scala.tools.testing.BytecodeTesting._
import scala.tools.testing.ClearAfterClass

@RunWith(classOf[JUnit4])
class UnreachableCodeTest extends ClearAfterClass {
  // jvm-1.6 enables emitting stack map frames, which impacts the code generation wrt dead basic blocks,
  // see comment in BCodeBodyBuilder
  val methodOptCompiler     = cached("methodOptCompiler", () => newCompiler(extraArgs = "-opt:l:method"))
  val dceCompiler           = cached("dceCompiler",       () => newCompiler(extraArgs = "-opt:unreachable-code"))
  val noOptCompiler         = cached("noOptCompiler",     () => newCompiler(extraArgs = "-opt:l:none"))

  def assertEliminateDead(code: (Instruction, Boolean)*): Unit = {
    val method = genMethod()(code.map(_._1): _*)
    dceCompiler.global.genBCode.bTypes.localOpt.removeUnreachableCodeImpl(method, "C")
    val nonEliminated = instructionsFromMethod(method)
    val expectedLive = code.filter(_._2).map(_._1).toList
    assertSameCode(nonEliminated, expectedLive)
  }

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
      // reachable, but removed anyway.
      Op(NOP).dead,
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
    val withDce = dceCompiler.compileInstructions(code)
    assertSameCode(withDce.dropNonOp, List(Op(ICONST_1), Op(IRETURN)))

    val noDce = noOptCompiler.compileInstructions(code)

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
  }

  @Test
  def eliminateDeadCatchBlocks(): Unit = {
    // the Label(1) is live: it's used in the local variable descriptor table (local variable "this" has a range from 0 to 1).
    def wrapInDefault(code: Instruction*) = List(Label(0), LineNumber(1, Label(0))) ::: code.toList ::: List(Label(1))

    val code = "def f: Int = { return 0; try { 1 } catch { case _: Exception => 2 } }"
    val m = dceCompiler.compileMethod(code)
    assertTrue(m.handlers.isEmpty) // redundant (if code is gone, handler is gone), but done once here for extra safety
    assertSameCode(m.instructions,
      wrapInDefault(Op(ICONST_0), Op(IRETURN)))

    val code2 = "def f: Unit = { try { } catch { case _: Exception => () }; () }"
    // requires fixpoint optimization of methodOptCompiler (dce alone is not enough): first the handler is eliminated, then it's dead catch block.
    assertSameCode(methodOptCompiler.compileInstructions(code2), wrapInDefault(Op(RETURN)))

    val code3 = "def f: Unit = { try { } catch { case _: Exception => try { } catch { case _: Exception => () } }; () }"
    assertSameCode(methodOptCompiler.compileInstructions(code3), wrapInDefault(Op(RETURN)))

    // this example requires two iterations to get rid of the outer handler.
    // the first iteration of DCE cannot remove the inner handler. then the inner (empty) handler is removed.
    // then the second iteration of DCE removes the inner catch block, and then the outer handler is removed.
    val code4 = "def f: Unit = { try { try { } catch { case _: Exception => () } } catch { case _: Exception => () }; () }"
    assertSameCode(methodOptCompiler.compileInstructions(code4), wrapInDefault(Op(RETURN)))
  }

  @Test // test the dce-testing tools
  def metaTest(): Unit = {
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
  def bytecodeEquivalence(): Unit = {
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

  @Test
  def loadNullNothingBytecode(): Unit = {
    val code =
      """class C {
        |  def nl: Null = null
        |  def nt: Nothing = throw new Error("")
        |  def cons(a: Any) = ()
        |
        |  def t1 = cons(null)
        |  def t2 = cons(nl)
        |  def t3 = cons(throw new Error(""))
        |  def t4 = cons(nt)
        |}
      """.stripMargin
    val c = noOptCompiler.compileClass(code)

    assertSameSummary(getMethod(c, "nl"), List(ACONST_NULL, ARETURN))

    assertSameSummary(getMethod(c, "nt"), List(
      NEW, DUP, LDC, "<init>", ATHROW))

    assertSameSummary(getMethod(c, "t1"), List(
      ALOAD, ACONST_NULL, "cons", RETURN))

    // GenBCode introduces POP; ACONST_NULL after loading an expression of type scala.runtime.Null$,
    // see comment in BCodeBodyBuilder.adapt
    assertSameSummary(getMethod(c, "t2"), List(
      ALOAD, ALOAD, "nl", POP, ACONST_NULL, "cons", RETURN))

    // the bytecode generated by GenBCode is ... ATHROW; INVOKEVIRTUAL C.cons; RETURN
    // the ASM classfile writer creates a new basic block (creates a label) right after the ATHROW
    // and replaces all instructions by NOP*; ATHROW, see comment in BCodeBodyBuilder.adapt
    // NOTE: DCE is enabled by default and gets rid of the redundant code (tested below)
    assertSameSummary(getMethod(c, "t3"), List(
      ALOAD, NEW, DUP, LDC, "<init>", ATHROW, NOP, NOP, NOP, ATHROW))

    // GenBCode introduces an ATHROW after the invocation of C.nt, see BCodeBodyBuilder.adapt
    // NOTE: DCE is enabled by default and gets rid of the redundant code (tested below)
    assertSameSummary(getMethod(c, "t4"), List(
      ALOAD, ALOAD, "nt", ATHROW, NOP, NOP, NOP, ATHROW))

    val cDCE = dceCompiler.compileClass(code)
    assertSameSummary(getMethod(cDCE, "t3"), List(ALOAD, NEW, DUP, LDC, "<init>", ATHROW))
    assertSameSummary(getMethod(cDCE, "t4"), List(ALOAD, ALOAD, "nt", ATHROW))
  }
}
