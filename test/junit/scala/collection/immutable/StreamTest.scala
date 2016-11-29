package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.ref.WeakReference
import scala.util.Try

@RunWith(classOf[JUnit4])
class StreamTest {

  @Test
  def t6727_and_t6440_and_8627(): Unit = {
    assertTrue(Stream.continually(()).filter(_ => true).take(2) == Seq((), ()))
    assertTrue(Stream.continually(()).filterNot(_ => false).take(2) == Seq((), ()))
    assertTrue(Stream(1,2,3,4,5).filter(_ < 4) == Seq(1,2,3))
    assertTrue(Stream(1,2,3,4,5).filterNot(_ > 4) == Seq(1,2,3,4))
    assertTrue(Stream.from(1).filter(_ > 4).take(3) == Seq(5,6,7))
    assertTrue(Stream.from(1).filterNot(_ <= 4).take(3) == Seq(5,6,7))
  }

  /** Test helper to verify that the given Stream operation allows
    * GC of the head during processing of the tail.
    */
  def assertStreamOpAllowsGC(op: (=> Stream[Int], Int => Unit) => Any, f: Int => Unit): Unit = {
    val msgSuccessGC = "GC success"
    val msgFailureGC = "GC failure"

    // A stream of 500 elements at most. We will test that the head can be collected
    // while processing the tail. After each element we will GC and wait 10 ms, so a
    // failure to collect will take roughly 5 seconds.
    val ref = WeakReference( Stream.from(1).take(500) )

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
  def foreach_allows_GC() {
    assertStreamOpAllowsGC(_.foreach(_), _ => ())
  }

  @Test
  def filter_all_foreach_allows_GC() {
    assertStreamOpAllowsGC(_.filter(_ => true).foreach(_), _ => ())
  }

  @Test // SI-8990
  def withFilter_after_first_foreach_allows_GC: Unit = {
    assertStreamOpAllowsGC(_.withFilter(_ > 1).foreach(_), _ => ())
  }

  @Test // SI-8990
  def withFilter_after_first_withFilter_foreach_allows_GC: Unit = {
    assertStreamOpAllowsGC(_.withFilter(_ > 1).withFilter(_ < 100).foreach(_), _ => ())
  }

  @Test // SI-8990
  def withFilter_can_retry_after_exception_thrown_in_filter: Unit = {
    // use mutable state to control an intermittent failure in filtering the Stream
    var shouldThrow = true

    val wf = Stream.from(1).take(10).withFilter { n =>
      if (shouldThrow && n == 5) throw new RuntimeException("n == 5") else n > 5
    }

    assertTrue( Try { wf.map(identity) }.isFailure ) // throws on n == 5

    shouldThrow = false                              // won't throw next time

    assertTrue( wf.map(identity).length == 5 )       // success instead of NPE
  }

  /** Test helper to verify that the given Stream operation is properly lazy in the tail */
  def assertStreamOpLazyInTail(op: (=> Stream[Int]) => Stream[Int], expectedEvaluated: List[Int]): Unit = {
    // mutable state to record every strict evaluation
    var evaluated: List[Int] = Nil

    def trackEffectsOnNaturals: Stream[Int] = {
      def loop(i: Int): Stream[Int] = { evaluated ++= List(i); i #:: loop(i + 1) }
      loop(1)
    }

    // call op on a stream which records every strict evaluation
    val result = op(trackEffectsOnNaturals)

    assertTrue( evaluated == expectedEvaluated )
  }

  @Test // SI-9134
  def filter_map_properly_lazy_in_tail: Unit = {
    assertStreamOpLazyInTail(_.filter(_ % 2 == 0).map(identity), List(1, 2))
  }

  @Test // SI-9134
  def withFilter_map_properly_lazy_in_tail: Unit = {
    assertStreamOpLazyInTail(_.withFilter(_ % 2 == 0).map(identity), List(1, 2))
  }

  @Test // SI-6881
  def test_reference_equality: Unit = {
    // Make sure we're tested with reference equality
    val s = Stream.from(0)
    assert(s == s, "Referentially identical streams should be equal (==)")
    assert(s equals s, "Referentially identical streams should be equal (equals)")
    assert((0 #:: 1 #:: s) == (0 #:: 1 #:: s), "Cons of referentially identical streams should be equal (==)")
    assert((0 #:: 1 #:: s) equals (0 #:: 1 #:: s), "Cons of referentially identical streams should be equal (equals)")
  }
}
