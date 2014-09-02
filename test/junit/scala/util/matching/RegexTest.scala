
package scala.util.matching

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class RegexTest {
  @Test def t8022CharSequence(): Unit = {
    val full = """.*: (.)$""".r
    val text = "   When I use this operator: *"
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val x :: Nil = full.unapplySeq(text: Any).get
    val y :: Nil = full.unapplySeq(text: CharSequence).get
    assertEquals("*", x)
    assertEquals("*", y)
  }

  @Test def t8022Match(): Unit = {
    val R = """(\d)""".r
    val matchh = R.findFirstMatchIn("a1").get
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val x :: Nil = R.unapplySeq(matchh: Any).get
    val y :: Nil = R.unapplySeq(matchh).get
    assertEquals("1", x)
    assertEquals("1", y)
  }

  @Test def t8787nullMatch() = {
    val r = """\d+""".r
    val s: String = null
    val x = s match { case r() => 1 ; case _ => 2 }
    assertEquals(2, x)
  }

  @Test def t8787nullMatcher() = {
    val r = """(\d+):(\d+)""".r
    val s = "1:2 3:4 5:6"
    val z = ((r findAllMatchIn s).toList :+ null) flatMap {
      case r(x, y) => Some((x.toInt, y.toInt))
      case _       => None
    }
    assertEquals(List((1,2),(3,4),(5,6)), z)
  }
}
