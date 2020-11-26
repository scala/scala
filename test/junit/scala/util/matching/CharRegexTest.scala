
package scala.util.matching

import org.junit.Test

import PartialFunction._

/** Regex can match a Char.
 *  If the pattern includes a group,
 *  always return a single char.
 */
class CharRegexTest {
  implicit class Averrable(val b: Boolean) /*extends AnyVal*/ {
    def yes(): Unit = assert(b)
    def no(): Unit = assert(!b)
  }
  val c: Char = 'c'  // "cat"(0)
  val d: Char = 'D'  // "Dog"(0)

  @Test def comparesGroupCorrectly(): Unit = {
    val r = """(\p{Lower})""".r
    cond(c) { case r(x) => true } .yes()
    cond(c) { case r(_) => true } .yes()
    cond(c) { case r(_*) => true } .yes()
    cond(c) { case r() => true } .no()

    cond(d) { case r(x) => true } .no()
    cond(d) { case r(_) => true } .no()
    cond(d) { case r(_*) => true } .no()
    cond(d) { case r() => true } .no()
  }

  @Test def comparesNoGroupCorrectly(): Unit = {
    val rnc = """\p{Lower}""".r
    cond(c) { case rnc(x) => true } .no()
    cond(c) { case rnc(_) => true } .no()
    cond(c) { case rnc(_*) => true } .yes()
    cond(c) { case rnc() => true } .yes()

    cond(d) { case rnc(x) => true } .no()
    cond(d) { case rnc(_) => true } .no()
    cond(d) { case rnc(_*) => true } .no()
    cond(d) { case rnc() => true } .no()
  }

  @Test(expected = classOf[MatchError])
  def failCorrectly(): Unit = {
    val headAndTail = """(\p{Lower})([a-z]+)""".r
    val n = "cat"(0) match {
      case headAndTail(ht @ _*) => ht.size
    }
    assert(false, s"Match size $n")
  }
}
