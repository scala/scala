package scala.tools.nsc
package backend.jvm
package opt

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.annotation.unused
import scala.tools.asm.Opcodes._
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting._

class SimplifyJumpsTest {
  @Test
  def simpleGotoReturn(): Unit = {
    val ops = List(
      Jump(GOTO, Label(2)), // replaced by RETURN
      Op(ICONST_1),         // need some code, otherwise removeJumpToSuccessor kicks in
      Op(POP),
      Label(1),             // multiple labels OK
      Label(2),
      Label(3),
      Op(RETURN)
    )
    val method = genMethod()(ops: _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), Op(RETURN) :: ops.tail)
  }

  @Test
  def simpleGotoThrow(): Unit = {
    val rest = List(
      Op(ICONST_1),         // need some code, otherwise removeJumpToSuccessor kicks in
      Op(POP),
      Label(1),
      Label(2),
      Label(3),
      Op(ATHROW)
    )
    val method = genMethod()(
      Op(ACONST_NULL) ::
      Jump(GOTO, Label(2)) :: // replaced by ATHROW
      rest: _*
    )
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), Op(ACONST_NULL) :: Op(ATHROW) :: rest)
  }

  @Test
  def gotoThrowInTry(): Unit = {
    val handler = List(ExceptionHandler(Label(1), Label(2), Label(4), Some("java/lang/Throwable")))
    val initialInstrs = List(
      Label(1),
      Op(ACONST_NULL),
      Jump(GOTO, Label(3)), // not by ATHROW (would move the ATHROW into a try block)
      Label(2),
      Op(ICONST_1),         // need some code, otherwise removeJumpToSuccessor kicks in
      Op(POP),
      Label(3),
      Op(ATHROW),
      Label(4),
      Op(POP),
      Op(RETURN)
    )
    val method = genMethod(handlers = handler)(initialInstrs: _*)
    assertFalse(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), initialInstrs)

    val optMethod = genMethod()(initialInstrs: _*) // no handler
    assertTrue(LocalOptImpls.simplifyJumps(optMethod))
    assertSameCode(instructionsFromMethod(optMethod).take(3), List(Label(1), Op(ACONST_NULL), Op(ATHROW)))
  }

  @Test
  def simplifyBranchOverGoto(): Unit = {
    val begin = List(
      VarOp(ILOAD, 1),
      Jump(IFGE, Label(2))
    )
    val rest = List(
      Jump(GOTO, Label(3)),
      Label(11), // other labels here are allowed
      Label(2),
      VarOp(ILOAD, 1),
      Op(RETURN),
      Label(3),
      VarOp(ILOAD, 1),
      Op(IRETURN)
    )
    val method = genMethod()(begin ::: rest: _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(
      instructionsFromMethod(method),
      List(VarOp(ILOAD, 1), Jump(IFLT, Label(3))) ::: rest.tail )

    // branch over goto is OK even if there's a label in between, if that label is not a jump target
    val withNonJumpTargetLabel = genMethod()(begin ::: Label(22) :: rest: _*)
    assertTrue(LocalOptImpls.simplifyJumps(withNonJumpTargetLabel))
    assertSameCode(
      instructionsFromMethod(withNonJumpTargetLabel),
      List(VarOp(ILOAD, 1), Jump(IFLT, Label(3)), Label(22)) ::: rest.tail )

    // if the Label(22) between IFGE and GOTO is the target of some jump, we cannot rewrite the IFGE
    // and remove the GOTO: removing the GOTO would change semantics. However, the jump that targets
    // Label(22) will be re-written (jump-chain collapsing), so in a second round, the IFGE is still
    // rewritten to IFLT
    val twoRounds = genMethod()(List(VarOp(ILOAD, 1), Jump(IFLE, Label(22))) ::: begin ::: Label(22) :: rest: _*)
    assertTrue(LocalOptImpls.simplifyJumps(twoRounds))
    assertSameCode(
      instructionsFromMethod(twoRounds),
      List(VarOp(ILOAD, 1), Jump(IFLE, Label(3)), VarOp(ILOAD, 1), Jump(IFLT, Label(3)), Label(22)) ::: rest.tail )
  }

  @Test
  def ensureGotoRemoved(): Unit = {
    def code(jumps: Instruction*) = List(
      VarOp(ILOAD, 1)) ::: jumps.toList ::: List(
      Label(2),

      Op(RETURN),
      Label(3),
      Op(RETURN)
    )

    // ensures that the goto is safely removed. ASM supports removing while iterating, but not the
    // next element of the current. Here, the current is the IFGE, the next is the GOTO.
    val method = genMethod()(code(Jump(IFGE, Label(2)), Jump(GOTO, Label(3))): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), code(Jump(IFLT, Label(3))))
  }

  @Test
  def removeJumpToSuccessor(): Unit = {
    val ops = List(
      Jump(GOTO, Label(1)),
      Label(11),
      Label(1),
      Label(2),
      VarOp(ILOAD, 1),
      Op(IRETURN)
    )
    val method = genMethod()(ops: _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops.tail)
  }

  @Test
  def collapseJumpChains(): Unit = {
    def ops(target1: Int, target2: Int, target3: Int) = List(
      VarOp(ILOAD, 1),
      Jump(IFGE, Label(target1)), // initially 1, then 3
      VarOp(ILOAD, 1),
      Op(IRETURN),

      Label(2),
      Jump(GOTO, Label(target3)),

      Label(1),
      Jump(GOTO, Label(target2)),     // initially 2, then 3

      VarOp(ILOAD, 1),                // some code to prevent jumpToSuccessor optimization (once target2 is replaced by 3)
      Op(RETURN),

      Label(3),
      VarOp(ILOAD, 1),
      Op(IRETURN)
    )
    val method = genMethod()(ops(1, 2, 3): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops(3, 3, 3))
  }

  @Test
  def collapseJumpChainLoop(): Unit = {
    def ops(target: Int) = List(
      VarOp(ILOAD, 1),
      Jump(IFGE, Label(target)),

      VarOp(ILOAD, 1), // some code to prevent rewriting the conditional jump
      Op(IRETURN),

      Label(4),
      Jump(GOTO, Label(3)),

      VarOp(ILOAD, 1), // some code to prevent jumpToSuccessor (label 3)
      Op(IRETURN),

      Label(3),
      Jump(GOTO, Label(4)),

      Label(2),
      Jump(GOTO, Label(3))
    )

    val method = genMethod()(ops(2): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops(3))
  }

  @Test
  def simplifyThenElseSameTarget(): Unit = {
    def ops(jumpOp: Instruction) = List(
      VarOp(ILOAD, 1),
      jumpOp,
      Label(2),
      Jump(GOTO, Label(1)),

      VarOp(ILOAD, 1), // some code to prevent jumpToSuccessor (label 1)
      Op(IRETURN),

      Label(1),
      VarOp(ILOAD, 1),
      Op(IRETURN)
    )

    val method = genMethod()(ops(Jump(IFGE, Label(1))): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops(Op(POP)))
  }

  @Test
  def thenElseSameTargetLoop(): Unit = {
    def ops(br: List[Instruction]) = List(
      VarOp(ILOAD, 1),
      VarOp(ILOAD, 2)) ::: br ::: List(
      Label(1),
      Jump(GOTO, Label(1))
    )
    val method = genMethod()(ops(List(Jump(IF_ICMPGE, Label(1)))): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops(List(Op(POP), Op(POP))))
  }

  @Test
  def simplifyIfEqConstTrue(): Unit = {
    def ops(br: List[Instruction]) = List(
      Op(ICONST_0)) ::: br ::: List(
      VarOp(ILOAD, 2),
      Label(1),
      Op(RETURN)
    )
    val method = genMethod()(ops(Jump(IFEQ, Label(1)) :: Nil): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops(Op(POP) :: Jump(GOTO, Label(1)) :: Nil))
  }

  @Test
  def simplifyIsNullConstFalse(): Unit = {
    def ops(br: List[Instruction]) = List(
      Op(ACONST_NULL)) ::: br ::: List(
      VarOp(ILOAD, 2),
      Label(1),
      Op(RETURN)
    )
    val method = genMethod()(ops(Jump(IFNONNULL, Label(1)) :: Nil): _*)
    assertTrue(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops(Op(POP) :: Nil))
  }

  @Test
  def noSimplifyNonConst(): Unit = {
    @unused val ops = List(
      Ldc(LDC, ""),
      Invoke(INVOKEVIRTUAL, "java/lang/String", "length", "()I", itf = false),
      Jump(IFEQ, Label(1)),
      Ldc(LDC, "nonempty"),
      Jump(GOTO, Label(2)),
      Label(1),
      Ldc(LDC, "empty"),
      Label(2),
      Op(RETURN)
    )
  }

  @Test
  def noSimplifyOverJumpTarget(): Unit = {
    val ops = List(
      Op(ACONST_NULL),
      Label(1),
      Jump(IFNULL, Label(2)),
      VarOp(ALOAD, 2),
      Jump(IFNULL, Label(1)),
      Label(2),
      Op(RETURN)
    )
    val method = genMethod()(ops: _*)
    assertFalse(LocalOptImpls.simplifyJumps(method))
    assertSameCode(instructionsFromMethod(method), ops)
  }
}
