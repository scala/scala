package scala.reflect.internal.util

import org.junit.Assert.{ assertThrows => _, _ }
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil._

@RunWith(classOf[JUnit4])
class SourceFileTest {
  def lineContentOf(code: String, offset: Int) =
    Position.offset(new BatchSourceFile("", code), offset).lineContent

  @Test
  def si8205_overflow(): Unit = {
    val file = new BatchSourceFile("", "code no newline")
    // the bug in lineToString counted until MaxValue, and the AIOOBE came from here
    assertFalse(file.isEndOfLine(Int.MaxValue))
  }

  @Test def si8630_lineToString(): Unit = {
    val code = "abc "
    assertEquals(code, new BatchSourceFile("", code).lineToString(0))
  }

  @Test
  def si8205_lineToString(): Unit = {
    assertEquals("", lineContentOf("", 0))
    assertEquals("abc", lineContentOf("abc", 0))
    assertEquals("abc", lineContentOf("abc", 3))
    assertEquals("code no newline", lineContentOf("code no newline", 1))
    assertEquals("", lineContentOf("\n", 0))
    assertEquals("abc", lineContentOf("abc\ndef", 0))
    assertEquals("abc", lineContentOf("abc\ndef", 3))
    assertEquals("def", lineContentOf("abc\ndef", 4))
    assertEquals("def", lineContentOf("abc\ndef", 6))
    assertEquals("def", lineContentOf("abc\ndef\n", 7))
  }

  @Test
  def CRisEOL(): Unit = {
    assertEquals("", lineContentOf("\r", 0))
    assertEquals("abc", lineContentOf("abc\rdef", 0))
    assertEquals("abc", lineContentOf("abc\rdef", 3))
    assertEquals("def", lineContentOf("abc\rdef", 4))
    assertEquals("def", lineContentOf("abc\rdef", 6))
    assertEquals("def", lineContentOf("abc\rdef\r", 7))
  }

  @Test
  def CRNLisEOL(): Unit = {
    assertEquals("", lineContentOf("\r\n", 0))
    assertEquals("abc", lineContentOf("abc\r\ndef", 0))
    assertEquals("abc", lineContentOf("abc\r\ndef", 3))
    assertEquals("abc", lineContentOf("abc\r\ndef", 4))
    assertEquals("def", lineContentOf("abc\r\ndef", 5))
    assertEquals("def", lineContentOf("abc\r\ndef", 7))
    assertEquals("def", lineContentOf("abc\r\ndef", 8))
    assertEquals("def", lineContentOf("abc\r\ndef\r\n", 9))
  }

  @Test def `t9885 lineToOffset throws on bad line`: Unit = {
    val text = "a\nb\nc\n"
    val f = new BatchSourceFile("batch", text)
    // EOL is line terminator, not line separator, so there is not an empty 4th line
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(3)
    }
    assertEquals(4, f.lineToOffset(2))

    // Position and SourceFile count differently
    val p = Position.offset(f, text.length - 1)
    val q = Position.offset(f, f.lineToOffset(p.line - 1))
    assertEquals(p.line, 3)
    assertEquals(p.line, q.line)
    assertEquals(p.column, q.column + 1)
    assertThrows[IndexOutOfBoundsException] {
      Position.offset(f, f.lineToOffset(p.line))
    }
  }
  @Test def `t9885 lineToOffset ignores lack of final EOL`: Unit = {
    val text = "a\nb\nc"
    val f = new BatchSourceFile("batch", text)
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(3)
    }
    assertEquals(4, f.lineToOffset(2))
    // final EOL is appended silently; this could throw OOB
    assertEquals(2, f.offsetToLine(text.length))
  }
  @Test def `t11572 offsetToLine throws on bad offset`: Unit = {
    val text = "a\nb\nc\n"
    val f = new BatchSourceFile("batch", text)
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(6)
    }
    assertThrows[IndexOutOfBoundsException] {
      f.offsetToLine(-1)
    }
    assertEquals(0, f.offsetToLine(0))
    assertEquals(0, f.offsetToLine(1))
    assertEquals(1, f.offsetToLine(2))
    assertEquals(2, f.offsetToLine(4))
    assertEquals(2, f.offsetToLine(5))
  }
}
