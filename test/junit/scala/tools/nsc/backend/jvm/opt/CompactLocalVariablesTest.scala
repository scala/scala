package scala.tools.nsc
package backend.jvm
package opt

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting._
import scala.tools.testkit.ClearAfterClass

class CompactLocalVariablesTest extends ClearAfterClass {
  // recurse-unreachable-jumps is required for eliminating catch blocks, in the first dce round they
  // are still live.only after eliminating the empty handler the catch blocks become unreachable.
  val methodOptCompiler     = cached("methodOptCompiler",     () => newCompiler(extraArgs = "-opt:unreachable-code,compact-locals"))
  val noCompactVarsCompiler = cached("noCompactVarsCompiler", () => newCompiler(extraArgs = "-opt:unreachable-code"))

  @Test
  def compactUnused(): Unit = {
    val code =
      """def f: Double = {
        |  try { }
        |  catch {
        |    case _: Throwable =>
        |      // eliminated by dce
        |      val i = 1
        |      val d = 1d
        |      val f = 1f
        |      val l = 1L
        |  }
        |
        |  val i = 1      // variable index 1 (it's an instance method, so at index 0 we have `this`)
        |  val d = 1d     // 2,3
        |  val f = 1f     // 4
        |  val l = 1L     // 5,6
        |
        |  try { }
        |  catch {
        |    case _: Throwable =>
        |      // eliminated by dce
        |      val i = 1
        |      val d = 1d
        |      val f = 1f
        |      val l = 1L
        |  }
        |
        |  val ii = 1     // 7
        |  val dd = 1d    // 8,9
        |  val ff = 1f    // 10
        |  val ll = 1L    // 11,12
        |
        |  i + ii + d + dd + f + ff + l + ll
        |}
        |""".stripMargin

    val noCompact   = noCompactVarsCompiler.compileAsmMethod(code)
    val withCompact = methodOptCompiler.compileAsmMethod(code)

    // code is the same, except for local var indices
    assertTrue(noCompact.instructions.size == withCompact.instructions.size)

    val varOpSlots = convertMethod(withCompact).instructions collect {
      case VarOp(_, v) => v
    }
    assertTrue(varOpSlots == List(1, 2, 4, 5, 7, 8, 10, 11,  // stores
                                                       1, 7, 2, 8, 4, 10, 5, 11), varOpSlots.toString) // loads

    // the local variables descriptor table is cleaned up to remove stale entries after dce,
    // also when the slots are not compacted
    assertTrue(noCompact.localVariables.size == withCompact.localVariables.size)

    assertTrue(noCompact.maxLocals == 25)
    assertTrue(withCompact.maxLocals == 13)
  }
}
