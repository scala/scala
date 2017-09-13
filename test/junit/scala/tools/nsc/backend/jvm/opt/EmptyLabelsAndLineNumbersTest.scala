package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters
import scala.tools.partest.ASMConverters._
import scala.tools.testing.AssertUtil._
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class EmptyLabelsAndLineNumbersTest {
  @Test
  def removeEmptyLineNumbers(): Unit = {
    val ops = List[(Instruction, Boolean)](
      Label(1),
      LineNumber(1, Label(1)),
      Label(2),
      Label(3),
      Op(RETURN),

      Label(4),
      LineNumber(4, Label(4)).dead,
      LineNumber(5, Label(4)),
      Op(RETURN),

      Label(5),
      LineNumber(6, Label(5)).dead,
      Label(6),
      Label(7),
      LineNumber(7, Label(7)),
      Op(RETURN),

      Label(9),
      LineNumber(8, Label(9)).dead,
      Label(10)
    )

    val method = genMethod()(ops.map(_._1): _*)
    assertTrue(LocalOptImpls.removeEmptyLineNumbers(method))
    assertSameCode(instructionsFromMethod(method), ops.filter(_._2).map(_._1))
  }

  @Test
  def badlyLocatedLineNumbers(): Unit = {
    def t(ops: Instruction*) =
      assertThrows[AssertionError](LocalOptImpls.removeEmptyLineNumbers(genMethod()(ops: _*)))

    // line numbers have to be right after their referenced label node
    t(LineNumber(0, Label(1)), Label(1))
    t(Label(0), Label(1), LineNumber(0, Label(0)))
  }
}
