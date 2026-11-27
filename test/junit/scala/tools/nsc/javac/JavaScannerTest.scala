package scala.tools.nsc.javac

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class JavaScannerTest extends BytecodeTesting {
  import compiler.global._

  private def names(code: String): List[String] = {
    new Run
    val unit = new CompilationUnit(new BatchSourceFile("J.java", code))
    val in = newJavaUnitParser(unit).in
    val names = ListBuffer.empty[String]
    while (in.token != JavaTokens.EOF) {
      if (in.token != JavaTokens.ERROR) names += in.name.toString
      in.nextToken()
    }
    names.toList
  }

  @Test
  def unclosedStringLiteralIsDiscarded(): Unit =
    assertEquals(List("next"), names("\"abc\nnext"))

  @Test
  def unclosedCharacterLiteralIsDiscarded(): Unit =
    assertEquals(List("b"), names("'ab"))
}
