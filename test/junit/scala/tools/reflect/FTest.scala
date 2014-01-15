
package scala.tools.reflect

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import PartialFunction._

/** Unit tests for `f` interpolator.
 */
@RunWith(classOf[JUnit4])
class FTest {
  @Test def qescape(): Unit = {
    assertEquals("\"hello, world\"", f"%qhello, world%q")
    assertEquals("hello, \"world\"", f"hello, %qworld%q")
    assertEquals("hello, \"world\"", f"${"hello, "}%qworld%q")
  }
}
