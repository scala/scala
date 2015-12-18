package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.analysis.{AliasingFrame, AliasingAnalyzer}

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._
import BackendReporting._
import BytecodeUtils._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object AnalyzerTest extends ClearAfterClass.Clearable {
  var noOptCompiler = newCompiler(extraArgs = "-Yopt:l:none")
  def clear(): Unit = { noOptCompiler = null }
}

@RunWith(classOf[JUnit4])
class AnalyzerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = AnalyzerTest
  val noOptCompiler = AnalyzerTest.noOptCompiler

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

    val List(c) = compileClasses(noOptCompiler)(code)
    val a = new AliasingAnalyzer(new BasicInterpreter)
    val f = findAsmMethod(c, "f")
    a.analyze("C", f)

    val List(_, i2l) = findInstr(f, "I2L")
    val aliasesAtI2l = a.frameAt(i2l, f).asInstanceOf[AliasingFrame[_]].aliases
    assertEquals(aliasesAtI2l(1).iterator.toList, List(1, 8, 9)) // a, e and stack top
    assertEquals(aliasesAtI2l(4).iterator.toList, List(4, 6))

    val List(add) = findInstr(f, "LADD")
    val aliasesAtAdd = a.frameAt(add, f).asInstanceOf[AliasingFrame[_]].aliases
    assertEquals(aliasesAtAdd(1).iterator.toList, List(1, 8)) // after i2l the value on the stack is no longer an alias
    assertEquals(aliasesAtAdd(4).iterator.toList, List(4, 6, 10)) // c, d and stack top
  }
}
