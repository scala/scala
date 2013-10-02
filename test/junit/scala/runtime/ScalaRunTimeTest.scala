package scala.runtime

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** Tests for the private class DefaultPromise */
@RunWith(classOf[JUnit4])
class ScalaRunTimeTest {
  @Test
  def testIsTuple() {
    import ScalaRunTime.isTuple
    def check(v: Any) = {
      assertTrue(v.toString, isTuple(v))
    }

    val s = ""
    check(Tuple1(s))
    check((s, s))
    check((s, s, s))
    check((s, s, s, s))
    check((s, s, s, s, s))
    check((s, s, s, s, s, s))
    check((s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))

    // some specialized variants will have mangled classnames
    check(Tuple1(0))
    check((0, 0))
    check((0, 0, 0))
    check((0, 0, 0, 0))
    check((0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

    case class C()
    val c = new C()
    assertFalse(c.toString, isTuple(c))
  }
}
