package scala.sys

import org.junit.Test
import org.junit.Assert._

import java.util.NoSuchElementException

import scala.util.Properties.isWin
import scala.tools.testkit.AssertUtil.assertThrows

class EnvTest {
  @Test def `env is case insensitive`(): Unit = {
    assertTrue(sys.env("PATH") != null)
    if (isWin) assertTrue(sys.env("path") != null)
  }
  @Test def `env throws if not found`(): Unit =
    assertThrows[NoSuchElementException](sys.env("junk"), _ == "junk")
}
