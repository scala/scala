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

  @Test
  def removeEmptyLabels(): Unit = {
    val handler = List(ExceptionHandler(Label(4), Label(5), Label(6), Some("java/lang/Throwable")))
    def ops(target1: Int, target2: Int, target3: Int, target4: Int, target5: Int, target6: Int) = List[(Instruction, Boolean)](
      Label(1),
      Label(2).dead,
      Label(3).dead,
      LineNumber(3, Label(target1)),
      VarOp(ILOAD, 1),
      Jump(IFGE, Label(target2)),

      Label(4),
      Label(5).dead,
      Label(6).dead,
      VarOp(ILOAD, 2),
      Jump(IFGE, Label(target3)),

      Label(7),
      Label(8).dead,
      Label(9).dead,
      Op(RETURN),

      LookupSwitch(LOOKUPSWITCH, Label(target4), List(1,2), List(Label(target4), Label(target5))),
      TableSwitch(TABLESWITCH, 1, 2, Label(target4), List(Label(target4), Label(target5))),

      Label(10),
      LineNumber(10, Label(10)),
      Label(11).dead,
      LineNumber(12, Label(target6))
    )

    val method = genMethod(handlers = handler)(ops(2, 3, 8, 8, 9, 11).map(_._1): _*)
    assertTrue(LocalOptImpls.removeEmptyLabelNodes(method))
    val m = convertMethod(method)
    assertSameCode(m.instructions, ops(1, 1, 7, 7, 7, 10).filter(_._2).map(_._1))
    assertTrue(m.handlers match {
      case List(ExceptionHandler(Label(4), Label(4), Label(4), _)) => true
      case _ => false
    })
  }
}
