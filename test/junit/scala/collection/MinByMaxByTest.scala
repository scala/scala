package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.annotation.unused
import scala.tools.testkit.AssertUtil.assertThrows
import scala.util.Random

/* Test for scala/bug#7614 */
class MinByMaxByTest {
  val list = List.fill(1000)(Random.nextInt(10000) - 5000)

  // Basic emptiness check
  @Test
  def checkEmpty(): Unit = {
    assertThrows[UnsupportedOperationException](List[Int]().maxBy(_ * 3))
    assertThrows[UnsupportedOperationException](List[Int]().minBy(_ * 3))
  }

  // Basic definition of minBy/maxBy.
  @Test
  def testCorrectness() = {
    def f(x: Int) = -1 * x
    val max = list.maxBy(f)
    assertTrue(list.forall(f(_) <= f(max)), "f(list.maxBy(f)) should ≥ f(x) where x is any element of list.")
    val min = list.minBy(f)
    assertTrue(list.forall(f(_) >= f(min)), "f(list.minBy(f)) should ≤ f(x) where x is any element of list.")
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

    @unused val max = list.maxBy(x => {
      evaluatedCountOfMaxBy += 1
      x * 10
    })
    assert(evaluatedCountOfMaxBy == list.length, s"maxBy: should evaluate f only ${list.length} times, but it evaluated $evaluatedCountOfMaxBy times.")

    var evaluatedCountOfMinBy = 0

    @unused val min = list.minBy(x => {
      evaluatedCountOfMinBy += 1
      x * 10
    })
    assert(evaluatedCountOfMinBy == list.length, s"minBy: should evaluate f only ${list.length} times, but it evaluated $evaluatedCountOfMinBy times.")
  }

  @Test
  def checkEmptyOption(): Unit = {
    assert(Seq.empty[Int].maxOption == None, "maxOption on a Empty Iterable is None")
    assert(Seq.empty[Int].minOption == None, "minOption on a Empty Iterable is None")
    assert(Seq.empty[Int].maxByOption(identity) == None, "maxByOption on a Empty Iterable is None")
    assert(Seq.empty[Int].minByOption(identity) == None, "minByOption on a Empty Iterable is None")
  }

  @Test
  def checkNonEmptyOption(): Unit = {
    assert(Seq(1).maxOption == Some(1), "maxOption on a Non Empty Iterable has value")
    assert(Seq(1).minOption == Some(1), "minOption on a Non Empty Iterable has value")
    assert(Seq(1).maxByOption(identity) == Some(1), "maxByOption on a Non Empty Iterable has value")
    assert(Seq(1).minByOption(identity) == Some(1), "minByOption on a Non Empty Iterable has value")
  }

  @Test
  def testMinMaxCorrectness(): Unit = {
    import Ordering.Double.IeeeOrdering
    val seq = Seq(5.0, 3.0, Double.NaN, 4.0)
    assert(seq.min.isNaN)
    assert(seq.max.isNaN)
  }

}
