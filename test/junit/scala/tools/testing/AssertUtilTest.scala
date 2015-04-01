package scala.tools
package testing

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import AssertUtil._

import java.lang.ref._

@RunWith(classOf[JUnit4])
class AssertUtilTest {

  @Test def reachableIgnoresReferences(): Unit = {
    class Holder[A](val ref: SoftReference[A])
    val o = new Object
    val r = new SoftReference(o)
    assertNotReachable(o, new Holder(r)) { }
  }
}
