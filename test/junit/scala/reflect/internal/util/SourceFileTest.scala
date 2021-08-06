package scala.reflect.internal.util

import org.junit.jupiter.api.Assertions.{assertThrows => _, _}
import org.junit.jupiter.api.Test

import scala.tools.testkit.AssertUtil._

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

  @Test def si9885_lineToOffset(): Unit = {
    val text = "a\nb\nc\n"
    val f = new BatchSourceFile("batch", text)
    assertThrows[IndexOutOfBoundsException] {
      f.lineToOffset(3)
    }
    assertEquals(4, f.lineToOffset(2))

    val p = Position.offset(f, text.length - 1)
    val q = Position.offset(f, f.lineToOffset(p.line - 1))
    assertEquals(p.line, q.line)
    assertEquals(p.column, q.column + 1)
    assertThrows[IndexOutOfBoundsException] {
      Position.offset(f, f.lineToOffset(p.line))
    }
  }
}
