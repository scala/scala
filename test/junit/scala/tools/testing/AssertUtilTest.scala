package scala.tools
package testing

import java.lang.ref._

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class AssertUtilTest {
  @Test def assertThrowsAssertion(): Unit = {
    assertThrows[AssertionError](throw new AssertionError("meme"), _ == "meme")
    try {
      assertThrows[AssertionError](())
      assert(false, "should have thrown!")
    } catch {
      case e: AssertionError if e.getMessage == "Expression did not throw!" =>
    }
  }

  @Test def reachableIgnoresReferences(): Unit = {
    class Holder[A](val ref: SoftReference[A])
    val o = new Object
    val r = new SoftReference(o)
    assertNotReachable(o, new Holder(r)) { }
  }

  @Test def reachableFollowArrays(): Unit = {
    class Holder[A](val ref: SoftReference[A])
    val o = new Object
    val r = new SoftReference(o)
    assertNotReachable(o, Array(new Holder(r))) { }
    assertNotReachable(o, Array(Array(r))) { }
    assertThrows[AssertionError](assertNotReachable(o, Array(Array(o))) { })
    assertThrows[AssertionError](assertNotReachable(o, new Object { val f = Array(o) }) { })
  }
}
