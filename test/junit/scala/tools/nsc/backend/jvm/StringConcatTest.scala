package scala.tools.nsc
package backend.jvm

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

object StringConcatTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler()
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class StringConcatTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = StringConcatTest
  val compiler = StringConcatTest.compiler

  val commonPreInstructions = List(Label(0), LineNumber(1, Label(0)), TypeOp(NEW, "java/lang/StringBuilder"), Op(DUP), Invoke(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false), Ldc(LDC, "abc"), Invoke(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false), VarOp(ALOAD, 0))

  val commonPostInstructions = List(Invoke(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false), Op(ARETURN), Label(12))

  def instructionsWithCommonParts(instructions: List[Instruction]) = commonPreInstructions ++ instructions ++ commonPostInstructions

  def instructionsForResultMethod(code: String): List[Instruction] = {
    val methods = compileMethods(compiler)(code)
    val resultMethod = methods.find(_.name == "result").get
    instructionsFromMethod(resultMethod)
  }

  @Test
  def concatStringToStringBuilder: Unit = {
    val code = """ def string = "def"; def result = "abc" + string """
    val actualInstructions = instructionsForResultMethod(code)
    val expectedInstructions = instructionsWithCommonParts(List(Invoke(INVOKEVIRTUAL, "C", "string", "()Ljava/lang/String;", false), Invoke(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false)))
    assertSameCode(actualInstructions, expectedInstructions)
  }

  @Test
  def concatStringBufferToStringBuilder: Unit = {
    val code = """ def stringBuffer = new java.lang.StringBuffer("def"); def result = "abc" + stringBuffer """
    val actualInstructions = instructionsForResultMethod(code)
    val expectedInstructions = instructionsWithCommonParts(List(Invoke(INVOKEVIRTUAL, "C", "stringBuffer", "()Ljava/lang/StringBuffer;", false), Invoke(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/StringBuffer;)Ljava/lang/StringBuilder;", false)))
    assertSameCode(actualInstructions, expectedInstructions)
  }

  @Test
  def concatCharSequenceToStringBuilder: Unit = {
    val code = """ def charSequence: CharSequence = "def"; def result = "abc" + charSequence """
    val actualInstructions = instructionsForResultMethod(code)
    val expectedInstructions = instructionsWithCommonParts(List(Invoke(INVOKEVIRTUAL, "C", "charSequence", "()Ljava/lang/CharSequence;", false), Invoke(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;", false)))
    assertSameCode(actualInstructions, expectedInstructions)
  }

  @Test
  def concatIntToStringBuilder: Unit = {
    val code = """ def int = 123; def result = "abc" + int """
    val actualInstructions = instructionsForResultMethod(code)
    val expectedInstructions = instructionsWithCommonParts(List(Invoke(INVOKEVIRTUAL, "C", "int", "()I", false), Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false), Invoke(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false)))
    assertSameCode(actualInstructions, expectedInstructions)
  }
}
