package scala.tools.nsc
package transform.patmat

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.nsc.backend.jvm.CodeGenTools
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import scala.tools.testing.ClearAfterClass

object PatmatBytecodeTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler()
  var optCompiler = newCompiler(extraArgs = "-Yopt:l:method")
  def clear(): Unit = { compiler = null; optCompiler = null }
}

@RunWith(classOf[JUnit4])
class PatmatBytecodeTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = PatmatBytecodeTest

  val compiler = PatmatBytecodeTest.compiler
  val optCompiler = PatmatBytecodeTest.optCompiler

  @Test
  def t6956(): Unit = {
    val code =
      """class C {
        |  private[this] final val ONE = 1
        |
        |  def s1(i: Byte): Int = i match {
        |    case ONE => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |
        |  def s2(i: Byte): Int = i match {
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    assert(getSingleMethod(c, "s1").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
    assert(getSingleMethod(c, "s2").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
  }

  @Test
  def t6955(): Unit = {
    val code =
      """class C {
        |  type Tag = Byte
        |
        |  def s1(i: Tag): Int = i match { // notice type of i is Tag = Byte
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |
        |  // this worked before, should keep working
        |  def s2(i: Byte): Int = i match {
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    assert(getSingleMethod(c, "s1").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
    assert(getSingleMethod(c, "s2").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
  }
}
