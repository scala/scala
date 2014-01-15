
package scala.util.matching

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class QuotedRegexTest {
  import PartialFunction.cond

  def aver[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertTrue(cond(x)(pf))
  def deny[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertFalse(cond(x)(pf))

  @Test def t8107_quoted(): Unit = {
    val r = "a*c".r.quoted
    deny("abc") { case r() => true }
    aver("a*c") { case r() => true }
    val u = "a*b".r.quoted.unanchored
    aver("aaa*bbb") { case u() => true }
    val v = "a*b".r.quoted.unanchored.unanchored
    aver("aaa*bbb") { case v() => true }
    val q = "a*b".r.unanchored.quoted
    aver("aaa*bbb") { case q() => true }
    val z = "a*c".r.quoted.quoted
    aver("a*c") { case z() => true }
    val y = "a*b".r.unanchored.quoted.quoted
    aver("aaa*bbb") { case y() => true }
  }

  @Test def anchorage(): Unit = {
    val v = "a*b".r.quoted.unanchored.anchored
    aver("a*b") { case v() => true }
    deny("aaa*bbb") { case v() => true }
  }

  /** Pattern is compiled lazily.
   *
   *  Pattern compile "[]" throws.
   */
  @Test def t8107_lazily(): Unit = {
    val q = "[]".r.quoted
    val r = s"Array$q".r
    aver("Array[]") { case r() => true }
  }
}
