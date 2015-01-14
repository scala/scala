package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.util.Random

@RunWith(classOf[JUnit4])
/* Test for SI-7614 */
class TraversableOnceTest {
  val list = List.fill(1000)(scala.util.Random.nextInt(10000) - 5000)

  // Basic emptiness check
  @Test
  def checkEmpty {
    def hasException(code: => Any): Boolean = try {
      code
      false
    } catch {
      case u: UnsupportedOperationException => true
      case t: Throwable => false
    }
    assert(hasException({ List[Int]().maxBy(_ * 3) }), "maxBy: on empty list should throw UnsupportedOperationException.")
    assert(hasException({ List[Int]().minBy(_ * 3) }), "minBy: on empty list should throw UnsupportedOperationException.")
  }

  // Basic definition of minBy/maxBy.
  @Test
  def testCorrectness() = {
    def f(x: Int) = -1 * x
    val max = list.maxBy(f)
    assert(list.forall(f(_) <= f(max)), "f(list.maxBy(f)) should ≥ f(x) where x is any element of list.")

    val min = list.minBy(f)
    assert(list.forall(f(_) >= f(min)), "f(list.minBy(f)) should ≤ f(x) where x is any element of list.")
  }

  // Ensure that it always returns the first match if more than one element have the same largest/smallest f(x).
  // Note that this behavior is not explicitly stated before. 
  // To make it compatible with the previous implementation, I add this behavior to docs.
  @Test
  def testReturnTheFirstMatch() = {
    val d = List(1, 2, 3, 4, 5, 6, 7, 8)
    def f(x: Int) = x % 3;
    assert(d.maxBy(f) == 2, "If multiple elements evaluated to the largest value, maxBy should return the first one.")
    assert(d.minBy(f) == 3, "If multiple elements evaluated to the largest value, minBy should return the first one.")
  }

  // Make sure it evaluates f no more than list.length times.
  @Test
  def testOnlyEvaluateOnce() = {
    var evaluatedCountOfMaxBy = 0

    val max = list.maxBy(x => {
      evaluatedCountOfMaxBy += 1
      x * 10
    })
    assert(evaluatedCountOfMaxBy == list.length, s"maxBy: should evaluate f only ${list.length} times, but it evaluated $evaluatedCountOfMaxBy times.")

    var evaluatedCountOfMinBy = 0

    val min = list.minBy(x => {
      evaluatedCountOfMinBy += 1
      x * 10
    })
    assert(evaluatedCountOfMinBy == list.length, s"minBy: should evaluate f only ${list.length} times, but it evaluated $evaluatedCountOfMinBy times.")
  }

}
