
package scala.util.matching

import scala.tools.testkit.AssertUtil.{assertCond, assertCondNot, assertThrows}

import org.junit.Test

/** Regex can match a Char.
 *  If the pattern includes a group,
 *  always return a single char.
 */
class CharRegexTest {

  val c: Char = 'c'  // "cat"(0)
  val d: Char = 'D'  // "Dog"(0)

  @Test def comparesGroupCorrectly: Unit = {
    val r = """(\p{Lower})""".r
    assertCond(c) { case r(x) => true }
    assertCond(c) { case r(_) => true }
    assertCond(c) { case r(_*) => true }
    assertCondNot(c) { case r() => true }

    assertCondNot(d) { case r(x) => true }
    assertCondNot(d) { case r(_) => true }
    assertCondNot(d) { case r(_*) => true }
    assertCondNot(d) { case r() => true }
  }

  @Test def comparesNoGroupCorrectly: Unit = {
    val rnc = """\p{Lower}""".r
    assertCondNot(c) { case rnc(x) => true }
    assertCondNot(c) { case rnc(_) => true }
    assertCond(c) { case rnc(_*) => true }
    assertCond(c) { case rnc() => true }

    assertCondNot(d) { case rnc(x) => true }
    assertCondNot(d) { case rnc(_) => true }
    assertCondNot(d) { case rnc(_*) => true }
    assertCondNot(d) { case rnc() => true }
  }

  @Test def failCorrectly: Unit = {
    val headAndTail = """(\p{Lower})([a-z]+)""".r
    def test = "cat"(0) match {
      case headAndTail(ht @ _*) => ht.size
    }
    assertThrows[MatchError](test)
  }
}
