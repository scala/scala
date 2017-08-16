package strawman.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.ref.WeakReference
import scala.util.Try

@RunWith(classOf[JUnit4])
class LazyListTest {

  @Test
  def t6727_and_t6440_and_8627(): Unit = {
    assertTrue(LazyList.continually(()).filter(_ => true).take(2) == Seq((), ()))
    assertTrue(LazyList.continually(()).filterNot(_ => false).take(2) == Seq((), ()))
    assertTrue(LazyList(1,2,3,4,5).filter(_ < 4) == Seq(1,2,3))
    assertTrue(LazyList(1,2,3,4,5).filterNot(_ > 4) == Seq(1,2,3,4))
    assertTrue(LazyList.from(1).filter(_ > 4).take(3) == Seq(5,6,7))
    assertTrue(LazyList.from(1).filterNot(_ <= 4).take(3) == Seq(5,6,7))
  }

  /** Test helper to verify that the given LazyList operation allows
    * GC of the head during processing of the tail.
    */
  def assertLazyListOpAllowsGC(op: (=> LazyList[Int], Int => Unit) => Any, f: Int => Unit): Unit = {
    val msgSuccessGC = "GC success"
    val msgFailureGC = "GC failure"

    // A LazyList of 500 elements at most. We will test that the head can be collected
    // while processing the tail. After each element we will GC and wait 10 ms, so a
    // failure to collect will take roughly 5 seconds.
    val ref = WeakReference( LazyList.from(1).take(500) )

    def gcAndThrowIfCollected(n: Int): Unit = {
      System.gc()                                                   // try to GC
      Thread.sleep(10)                                              // give it 10 ms
      if (ref.get.isEmpty) throw new RuntimeException(msgSuccessGC) // we're done if head collected
      f(n)
    }

    val res = Try { op(ref(), gcAndThrowIfCollected) }.failed       // success is indicated by an
    val msg = res.map(_.getMessage).getOrElse(msgFailureGC)         // exception with expected message 
                                                                    // failure is indicated by no
    assertTrue(msg == msgSuccessGC)                                 // exception, or one with different message
  }

  @Test
  def foreach_allows_GC(): Unit = {
    assertLazyListOpAllowsGC(_.foreach(_), _ => ())
  }

  @Test
  def filter_all_foreach_allows_GC(): Unit = {
    assertLazyListOpAllowsGC(_.filter(_ => true).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_after_first_foreach_allows_GC: Unit = {
    assertLazyListOpAllowsGC(_.withFilter(_ > 1).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_after_first_withFilter_foreach_allows_GC: Unit = {
    assertLazyListOpAllowsGC(_.withFilter(_ > 1).withFilter(_ < 100).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_can_retry_after_exception_thrown_in_filter: Unit = {
    // use mutable state to control an intermittent failure in filtering the LazyList
    var shouldThrow = true

    val wf = LazyList.from(1).take(10).withFilter { n =>
      if (shouldThrow && n == 5) throw new RuntimeException("n == 5") else n > 5
    }

    assertEquals(true, Try { wf.map(identity) }.isFailure) // throws on n == 5

    shouldThrow = false                              // won't throw next time

    assertEquals(5,  wf.map(identity).length)       // success instead of NPE
  }

  /** Test helper to verify that the given LazyList operation is properly lazy in the tail */
  def assertLazyListOpLazyInTail(op: (=> LazyList[Int]) => LazyList[Int], expectedEvaluated: List[Int]): Unit = {
    // mutable state to record every strict evaluation
    var evaluated: List[Int] = Nil

    def trackEffectsOnNaturals: LazyList[Int] = {
      def loop(i: Int): LazyList[Int] = { evaluated ++= List(i); LazyList.cons(i, loop(i + 1)) }
      loop(1)
    }

    // call op on a LazyList which records every strict evaluation
    val result = op(trackEffectsOnNaturals)

    assertEquals(expectedEvaluated, evaluated)
  }

  @Test // scala/bug#9134
  def filter_map_properly_lazy_in_tail: Unit = {
    assertLazyListOpLazyInTail(_.filter(_ % 2 == 0).map(identity), List(1, 2))
  }

  @Test // scala/bug#9134
  def withFilter_map_properly_lazy_in_tail: Unit = {
    assertLazyListOpLazyInTail(_.withFilter(_ % 2 == 0).map(identity), List(1, 2))
  }

  @Test // scala/bug#6881
  def test_reference_equality: Unit = {
    // Make sure we're tested with reference equality
    val s = LazyList.from(0)
    assert(s == s, "Referentially identical LazyLists should be equal (==)")
    assert(s equals s, "Referentially identical LazyLists should be equal (equals)")
    assert((0 #:: 1 #:: s) == (0 #:: 1 #:: s), "Cons of referentially identical LazyLists should be equal (==)")
    assert((0 #:: 1 #:: s) equals (0 #:: 1 #:: s), "Cons of referentially identical LazyLists should be equal (equals)")
  }

  @Test
  def t9886: Unit = {
    assertEquals(LazyList(None, Some(1)), None #:: LazyList(Some(1)))
    assertEquals(LazyList(None, Some(1)), LazyList(None) #::: LazyList(Some(1)))
  }
}
