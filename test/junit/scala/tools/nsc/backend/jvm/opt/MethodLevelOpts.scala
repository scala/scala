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
import scala.tools.testing.ClearAfterClass

object MethodLevelOpts extends ClearAfterClass.Clearable {
  var methodOptCompiler = newCompiler(extraArgs = "-target:jvm-1.6 -Ybackend:GenBCode -Yopt:l:method")
  def clear(): Unit = { methodOptCompiler = null }
}

@RunWith(classOf[JUnit4])
class MethodLevelOpts extends ClearAfterClass {
  ClearAfterClass.stateToClear = MethodLevelOpts

  val methodOptCompiler = MethodLevelOpts.methodOptCompiler

  def wrapInDefault(code: Instruction*) = List(Label(0), LineNumber(1, Label(0))) ::: code.toList ::: List(Label(1))

  @Test
  def eliminateEmptyTry(): Unit = {
    val code = "def f = { try {} catch { case _: Throwable => 0; () }; 1 }"
    val warn = "a pure expression does nothing in statement position"
    assertSameCode(singleMethodInstructions(methodOptCompiler)(code, allowMessage = _.msg contains warn), wrapInDefault(Op(ICONST_1), Op(IRETURN)))
  }

  @Test
  def cannotEliminateLoadBoxedUnit(): Unit = {
    // the compiler inserts a boxed into the try block. it's therefore non-empty (and live) and not eliminated.
    val code = "def f = { try {} catch { case _: Throwable => 0 }; 1 }"
    val m = singleMethod(methodOptCompiler)(code)
    assertTrue(m.handlers.length == 1)
    assertSameCode(m.instructions.take(3), List(Label(0), LineNumber(1, Label(0)), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;")))
  }

  @Test
  def inlineThrowInCatchNotTry(): Unit = {
    // the try block does not contain the `ATHROW` instruction, but in the catch block, `ATHROW` is inlined
    val code = "def f(e: Exception) = throw { try e catch { case _: Throwable => e } }"
    val m = singleMethod(methodOptCompiler)(code)
    assertHandlerLabelPostions(m.handlers.head, m.instructions, 0, 3, 5)
    assertSameCode(m.instructions,
      wrapInDefault(VarOp(ALOAD, 1), Label(3), Op(ATHROW), Label(5), FrameEntry(4, List(), List("java/lang/Throwable")), Op(POP), VarOp(ALOAD, 1), Op(ATHROW))
    )
  }

  @Test
  def inlineReturnInCatchNotTry(): Unit = {
    val code = "def f: Int = return { try 1 catch { case _: Throwable => 2 } }"
    // cannot inline the IRETURN into the try block (because RETURN may throw IllegalMonitorState)
    val m = singleMethod(methodOptCompiler)(code)
    assertHandlerLabelPostions(m.handlers.head, m.instructions, 0, 3, 5)
    assertSameCode(m.instructions,
      wrapInDefault(Op(ICONST_1), Label(3), Op(IRETURN), Label(5), FrameEntry(4, List(), List("java/lang/Throwable")), Op(POP), Op(ICONST_2), Op(IRETURN)))
  }

  @Test
  def simplifyJumpsInTryCatchFinally(): Unit = {
    val code =
      """def f: Int =
        |  try {
        |    return 1
        |  } catch {
        |    case _: Throwable =>
        |    return 2
        |  } finally {
        |    return 2
        |    // dead
        |    val x = try 10 catch { case _: Throwable => 11 }
        |    println(x)
        |  }
      """.stripMargin
    val m = singleMethod(methodOptCompiler)(code)
    assertTrue(m.handlers.length == 2)
    assertSameCode(m.instructions.dropNonOp, // drop line numbers and labels that are only used by line numbers

      // one single label left :-)
      List(Op(ICONST_1), VarOp(ISTORE, 2), Jump(GOTO, Label(20)), Op(POP), Op(ICONST_2), VarOp(ISTORE, 2), Jump(GOTO, Label(20)), VarOp(ASTORE, 3), Op(ICONST_2), Op(IRETURN), Label(20), Op(ICONST_2), Op(IRETURN))
    )
  }
}
