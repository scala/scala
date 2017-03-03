package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.analysis.{AliasingAnalyzer, AliasingFrame}
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class AnalyzerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._

  @Test
  def aliasingOfPrimitives(): Unit = {
    val code =
      """class C {
        |  def f(a: Int, b: Long) = {
        |    val c = a - b // a is converted with i2l
        |    val d = c
        |    val e = a
        |    // locals: 0 this -- 1 a -- 2-3 b -- 4-5 c -- 6-7 d -- 8 e
        |    e + d // e is converted with i2l
        |  }
        |}
      """.stripMargin

    val c = compileClass(code)
    val a = new AliasingAnalyzer(new BasicInterpreter)
    val f = getAsmMethod(c, "f")
    a.analyze("C", f)

    val List(_, i2l) = findInstrs(f, "I2L")
    val aliasesAtI2l = a.frameAt(i2l, f).asInstanceOf[AliasingFrame[_]].aliases
    assertEquals(aliasesAtI2l(1).iterator.toList, List(1, 8, 9)) // a, e and stack top
    assertEquals(aliasesAtI2l(4).iterator.toList, List(4, 6))

    val add = findInstr(f, "LADD")
    val aliasesAtAdd = a.frameAt(add, f).asInstanceOf[AliasingFrame[_]].aliases
    assertEquals(aliasesAtAdd(1).iterator.toList, List(1, 8)) // after i2l the value on the stack is no longer an alias
    assertEquals(aliasesAtAdd(4).iterator.toList, List(4, 6, 10)) // c, d and stack top
  }
}
