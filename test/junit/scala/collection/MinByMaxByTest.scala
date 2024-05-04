package scala.collection

import org.junit.Assert._
import org.junit.Test

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
    assertTrue("f(list.maxBy(f)) should ≥ f(x) where x is any element of list.", list.forall(f(_) <= f(max)))
    val min = list.minBy(f)
    assertTrue("f(list.minBy(f)) should ≤ f(x) where x is any element of list.", list.forall(f(_) >= f(min)))
  }

  // Ensure that it always returns the first match if more than one element have the same largest/smallest f(x).
  // Note that this behavior is not explicitly stated before. 
  // To make it compatible with the previous implementation, I add this behavior to docs.
  @Test
  def testReturnTheFirstMatch() = {
    val d = List(1, 2, 3, 4, 5, 6, 7, 8)
    def f(x: Int) = x % 3;
    assertEquals("If multiple elements evaluated to the largest value, maxBy should return the first one.",
      2, d.maxBy(f))
    assertEquals("If multiple elements evaluated to the largest value, minBy should return the first one.",
      3, d.minBy(f))
  }

  // Make sure it evaluates f no more than list.length times.
  @Test
  def testOnlyEvaluateOnce() = {
    var evaluatedCountOfMaxBy = 0

    val max = list.maxBy { x =>
      evaluatedCountOfMaxBy += 1
      x * 10
    }
    assertTrue(!list.exists(_ > max))
    assertEquals(s"maxBy: should evaluate f only ${list.length} times, but it evaluated $evaluatedCountOfMaxBy times.",
      list.length,
      evaluatedCountOfMaxBy)

    var evaluatedCountOfMinBy = 0

    val min = list.minBy { x =>
      evaluatedCountOfMinBy += 1
      x * 10
    }
    assertTrue(!list.exists(_ < min))
    assertEquals(s"minBy: should evaluate f only ${list.length} times, but it evaluated $evaluatedCountOfMinBy times.",
      list.length,
      evaluatedCountOfMinBy)
  }

  @Test
  def checkEmptyOption(): Unit = {
    assertTrue("maxOption on a Empty Iterable is None", Seq.empty[Int].maxOption.isEmpty)
    assertTrue("minOption on a Empty Iterable is None", Seq.empty[Int].minOption.isEmpty)
    assertTrue("maxByOption on a Empty Iterable is None", Seq.empty[Int].maxByOption(identity).isEmpty)
    assertTrue("minByOption on a Empty Iterable is None", Seq.empty[Int].minByOption(identity).isEmpty)
  }

  @Test
  def checkNonEmptyOption(): Unit = {
    assertEquals("maxOption on a Non Empty Iterable has value", Some(1), Seq(1).maxOption)
    assertEquals("minOption on a Non Empty Iterable has value", Some(1), Seq(1).minOption)
    assertEquals("maxByOption on a Non Empty Iterable has value", Some(1), Seq(1).maxByOption(identity))
    assertEquals("minByOption on a Non Empty Iterable has value", Some(1), Seq(1).minByOption(identity))
  }

  @Test
  def testMinMaxCorrectness(): Unit = {
    import Ordering.Double.IeeeOrdering
    val seq = Seq(5.0, 3.0, Double.NaN, 4.0)
    assertTrue(seq.min.isNaN)
    assertTrue(seq.max.isNaN)
  }

}
