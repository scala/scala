
package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class StringContextTest {

  import StringContext._

  @Test def noEscape() = {
    val s = "string"
    val res = processEscapes(s)
    assertEquals(s, res)
  }
  @Test def tabbed() = {
    val s = """a\tb"""
    val res = processEscapes(s)
    assertEquals("a\tb", res)
  }
  @Test def quoted() = {
    val s = """hello, \"world\""""
    val res = processEscapes(s)
    assertEquals("""hello, "world"""", res)
  }
  @Test def octal() = {
    val s = """\123cala"""
    val res = treatEscapes(s)
    assertEquals("Scala", res)
  }
  @Test def doubled() = {
    val s = """\123cala\123yntax"""
    val res = treatEscapes(s)
    assertEquals("ScalaSyntax", res)
  }
  @Test def badly() = assertThrows[InvalidEscapeException] {
    val s = """Scala\"""
    val res = treatEscapes(s)
    assertEquals("Scala", res)
  }
  @Test def noOctal() = assertThrows[InvalidEscapeException] {
    val s = """\123cala"""
    val res = processEscapes(s)
    assertEquals("Scala", res)
  }

  @Test def t5856(): Unit = {
    class X {
      override def toString = "Test"

      assertEquals("Test", s"$this")
      assertEquals("TestTest", s"$this$this")
      assertEquals("Test$", s"$this$$")
      assertEquals("Test.##", s"$this.##")
      assertEquals("Test.toString", s"$this.toString")
      assertEquals("Test=THIS", s"$this=THIS")
    }
    new X
  }
  @Test def t6476(): Unit = {
    assertEquals("\"Hello, world.\"", s"$"Hello, world.$"")
    val foo   = 42
    val foo_  = 43
    val foo_* = 44
    assertEquals("44", s"$foo_*")
    assertEquals("42_*", s"$`foo`_*")
    assertEquals("43=", s"$`foo_`=")

    val foo$bar = 3
    val bar = "skidoo"
    assertEquals("<3", s"<$`foo$bar`")
    assertEquals("42skidoo", s"$foo$bar")
  }
}
