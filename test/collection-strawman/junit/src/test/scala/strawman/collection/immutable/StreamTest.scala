package strawman.collection.immutable

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

    // A Stream of 500 elements at most. We will test that the head can be collected
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
  def foreach_allows_GC(): Unit = {
    assertStreamOpAllowsGC(_.foreach(_), _ => ())
  }

  @Test
  def filter_all_foreach_allows_GC(): Unit = {
    assertStreamOpAllowsGC(_.filter(_ => true).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_after_first_foreach_allows_GC: Unit = {
    assertStreamOpAllowsGC(_.withFilter(_ > 1).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_after_first_withFilter_foreach_allows_GC: Unit = {
    assertStreamOpAllowsGC(_.withFilter(_ > 1).withFilter(_ < 100).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_can_retry_after_exception_thrown_in_filter: Unit = {
    // use mutable state to control an intermittent failure in filtering the Stream
    var shouldThrow = true

    val wf = Stream.from(1).take(10).withFilter { n =>
      if (shouldThrow && n == 5) throw new RuntimeException("n == 5") else n > 5
    }

    assertEquals(true, Try { wf.map(identity) }.isFailure) // throws on n == 5

    shouldThrow = false                              // won't throw next time

    assertEquals(5,  wf.map(identity).length)       // success instead of NPE
  }

  /** Test helper to verify that the given Stream operation is properly lazy in the tail */
  def assertStreamOpLazyInTail(op: (=> Stream[Int]) => Stream[Int], expectedEvaluated: List[Int]): Unit = {
    // mutable state to record every strict evaluation
    var evaluated: List[Int] = Nil

    def trackEffectsOnNaturals: Stream[Int] = {
      def loop(i: Int): Stream[Int] = { evaluated ++= List(i); Stream.cons(i, loop(i + 1)) }
      loop(1)
    }

    // call op on a Stream which records every strict evaluation
    val result = op(trackEffectsOnNaturals)

    assertEquals(expectedEvaluated, evaluated)
  }

  @Test // scala/bug#9134
  def filter_map_properly_lazy_in_tail: Unit = {
    assertStreamOpLazyInTail(_.filter(_ % 2 == 0).map(identity), List(1, 2))
  }

  @Test // scala/bug#9134
  def withFilter_map_properly_lazy_in_tail: Unit = {
    assertStreamOpLazyInTail(_.withFilter(_ % 2 == 0).map(identity), List(1, 2))
  }

  @Test // scala/bug#6881
  def test_reference_equality: Unit = {
    // Make sure we're tested with reference equality
    val s = Stream.from(0)
    assert(s == s, "Referentially identical Streams should be equal (==)")
    assert(s equals s, "Referentially identical Streams should be equal (equals)")
    assert((0 #:: 1 #:: s) == (0 #:: 1 #:: s), "Cons of referentially identical Streams should be equal (==)")
    assert((0 #:: 1 #:: s) equals (0 #:: 1 #:: s), "Cons of referentially identical Streams should be equal (equals)")
  }

  @Test
  def t9886: Unit = {
    assertEquals(Stream(None, Some(1)), None #:: Stream(Some(1)))
    assertEquals(Stream(None, Some(1)), Stream(None) #::: Stream(Some(1)))
  }

  @Test
  def testForceReturnsEvaluatedStream() : Unit = {
    var i = 0
    def f: Int = { i += 1; i }
    val xs = f #:: f #:: f #:: Stream.empty
    assertEquals(1, i)
    xs.force
    assertEquals(3, i)
    // it's possible to implement `force` with incorrect string representation
    // (to forget about `tlEvaluated` update)
    assertEquals( "Stream(1, 2, 3)", xs.toString())
  }

  val cycle1: Stream[Int] = 1 #:: 2 #:: cycle1
  val cycle2: Stream[Int] = 1 #:: 2 #:: 3 #:: cycle2
  @Test(timeout=10000)
  def testSameElements(): Unit = {
    assert(Stream().sameElements(Stream()))
    assert(!Stream().sameElements(Stream(1)))
    assert(Stream(1,2).sameElements(Stream(1,2)))
    assert(!Stream(1,2).sameElements(Stream(1)))
    assert(!Stream(1).sameElements(Stream(1,2)))
    assert(!Stream(1).sameElements(Stream(2)))
    assert(!cycle1.sameElements(cycle2))
    assert(!cycle1.sameElements(cycle2))
  }

  @Test
  def testStreamToStringWhenHeadAndTailBothAreNotEvaluated = {
    val l = Stream(1, 2, 3, 4, 5)
    assertEquals("Stream(1, ?)", l.toString)
  }

  @Test
  def testStreamToStringWhenOnlyHeadIsEvaluated = {
    val l = Stream(1, 2, 3, 4, 5)
    l.head
    assertEquals("Stream(1, ?)", l.toString)
  }

  @Test
  def testStreamToStringWhenHeadAndTailIsEvaluated = {
    val l = Stream(1, 2, 3, 4, 5)
    l.head
    l.tail
    assertEquals("Stream(1, 2, ?)", l.toString)
  }

  @Test
  def testStreamToStringWhenHeadAndTailHeadIsEvaluated = {
    val l = Stream(1, 2, 3, 4, 5)
    l.head
    l.tail.head
    assertEquals("Stream(1, 2, ?)", l.toString)
  }

  @Test
  def testStreamToStringWhenHeadIsNotEvaluatedAndOnlyTailIsEvaluated = {
    val l = Stream(1, 2, 3, 4, 5)
    l.tail
    assertEquals("Stream(1, 2, ?)", l.toString)
  }

  @Test
  def testStreamToStringWhedHeadIsNotEvaluatedAndTailHeadIsEvaluated = {
    val l = Stream(1, 2, 3, 4, 5)
    l.tail.head
    assertEquals("Stream(1, 2, ?)", l.toString)
  }

  @Test
  def testStreamToStringWhenStreamIsForcedToList: Unit = {
    val l = 1 #:: 2 #:: 3 #:: 4 #:: Stream.empty
    l.toList
    assertEquals("Stream(1, 2, 3, 4)", l.toString)
  }

  @Test
  def testStreamToStringWhenStreamIsEmpty: Unit = {
    val l = Stream.empty
    assertEquals("Stream()", l.toString)
  }

  @Test
  def testStreamToStringWhenStreamHasCyclicReference: Unit = {
    lazy val cyc: Stream[Int] = 1 #:: 2 #:: 3 #:: 4 #:: cyc
    cyc.tail.tail.tail.tail
    assertEquals("Stream(1, 2, 3, 4, ...)", cyc.toString)
  }
}
