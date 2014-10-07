package scala.issues

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes
import scala.tools.nsc.backend.jvm.CodeGenTools._
import org.junit.Assert._
import scala.collection.JavaConverters._
import scala.tools.partest.ASMConverters._

@RunWith(classOf[JUnit4])
class BytecodeTests {
  val compiler = newCompiler()

  @Test
  def t8731(): Unit = {
    val code =
      """class C {
        |  def f(x: Int) = (x: @annotation.switch) match {
        |    case 1 => 0
        |    case 2 => 1
        |    case 3 => 2
        |  }
        |  final val K = 10
        |  def g(x: Int) = (x: @annotation.switch) match {
        |    case K => 0
        |    case 1 => 10
        |    case 2 => 20
        |  }
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)

    assertTrue(getSingleMethod(c, "f").instructions.count(_.isInstanceOf[TableSwitch]) == 1)
    assertTrue(getSingleMethod(c, "g").instructions.count(_.isInstanceOf[LookupSwitch]) == 1)
  }
}
