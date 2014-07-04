
package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class StringContextSupplementTest {

  import StringContext._

  @Test def t6631_baseline() = assertEquals("\f\r\n\t", s"""\f\r\n\t""")

  @Test def t6631_badEscape() = assertThrows[InvalidEscapeException] {
    s"""\x"""
  }

  @Test def antiHijack_?() = {
    object AllYourStringsAreBelongToMe { case class StringContext(args: Any*) { def s(args: Any) = "!!!!" } }
    import AllYourStringsAreBelongToMe._
    //assertEquals("????", s"????")
    assertEquals("!!!!", s"????")   // OK to hijack core interpolator ids
  }

  @Test def t6476() {
    val x = 42
    assertThrows[InvalidEscapeException] {
      Console println s"abc\$x"      // rt
    }
    assertThrows[InvalidEscapeException] {
      Console println s"""abc\$x"""  // rt
    }
    assertThrows[InvalidEscapeException] {
      Console println s"\"           // rt
    }
    assertThrows[InvalidEscapeException] {
      Console println s"""\"""       // rt
    }
  }
}
