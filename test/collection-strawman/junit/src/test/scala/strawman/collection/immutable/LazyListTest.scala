package strawman.collection.immutable

import strawman.collection.Iterator
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Test, Ignore}
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

  @Test
  def testLazyListDoesNotForceHead: Unit = {
    var i = 0
    def f: Int = { i += 1; i }
    val s = LazyList.empty.#::(f).#::(f).#::(f)
    assertEquals(0, i)
  }

  @Test
  def testEmptyLazyListToString(): Unit = {
    assertEquals("LazyList()", LazyList.Empty.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailBothAreNotEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    assertEquals("LazyList(_, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenOnlyHeadIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.head
    assertEquals("LazyList(1, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.head
    l.tail
    assertEquals("LazyList(1, _, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailHeadIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.head
    l.tail.head
    assertEquals("LazyList(1, 2, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadIsNotEvaluatedAndOnlyTailIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail
    assertEquals("LazyList(_, _, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhendHeadIsNotEvaluatedAndTailHeadIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.head
    assertEquals("LazyList(_, 2, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhendHeadIsNotEvaluatedAndTailTailIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.tail
    assertEquals("LazyList(_, _, _, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhendHeadIsNotEvaluatedAndTailTailHeadIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.tail.head
    assertEquals("LazyList(_, _, 3, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListIsForcedToList: Unit = {
    val l = 1 #:: 2 #:: 3 #:: 4 #:: LazyList.Empty
    l.toList
    assertEquals("LazyList(1, 2, 3, 4)", l.toString)
  }

  @Test
  def testLazyListToStringWhenStreamIsEmpty: Unit = {
    val l = LazyList.empty
    assertEquals("LazyList()", l.toString)
  }

  @Test
  def testLazyListToStringForSingleElementList: Unit = {
    val l = LazyList(1)
    l.force
    assertEquals("LazyList(1)", l.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListHasCyclicReference: Unit = {
    lazy val cyc: LazyList[Int] = 1 #:: 2 #:: 3 #:: 4 #:: cyc
    assertEquals("LazyList(_, ?)", cyc.toString)
    cyc.head
    assertEquals("LazyList(1, ?)", cyc.toString)
    cyc.tail
    assertEquals("LazyList(1, _, ?)", cyc.toString)
    cyc.tail.head
    assertEquals("LazyList(1, 2, ?)", cyc.toString)
    cyc.tail.tail.head
    assertEquals("LazyList(1, 2, 3, ?)", cyc.toString)
    cyc.tail.tail.tail.head
    assertEquals("LazyList(1, 2, 3, 4, ?)", cyc.toString)
    cyc.tail.tail.tail.tail.head
    assertEquals("LazyList(1, 2, 3, 4, ...)", cyc.toString)
  }

  def hasCorrectDrop(): Unit = {
    assertEquals(LazyList(), LazyList().drop(2))
    assertEquals(LazyList(), LazyList(1).drop(2))
    assertEquals(LazyList(), LazyList(1, 2).drop(2))
    assertEquals(LazyList(3), LazyList(1, 2, 3).drop(2))
    assertEquals(LazyList(3, 4), LazyList(1, 2, 3, 4).drop(2))
  }

  @Test
  def testForceReturnsEvaluatedLazyList() : Unit = {
    var i = 0
    def f: Int = { i += 1; i }
    val xs = LazyList.from(Iterator.fill(3)(f))
    assertEquals(0, i)
    xs.force
    assertEquals(3, i)
    // it's possible to implement `force` with incorrect string representation
    // (to forget about `tlEvaluated` update)
    assertEquals( "LazyList(1, 2, 3)", xs.toString())
  }

  val cycle1: LazyList[Int] = 1 #:: 2 #:: cycle1
  val cycle2: LazyList[Int] = 1 #:: 2 #:: 3 #:: cycle2
  @Test(timeout=10000)
  def testSameElements(): Unit = {
    assert(LazyList().sameElements(LazyList()))
    assert(!LazyList().sameElements(LazyList(1)))
    assert(LazyList(1,2).sameElements(LazyList(1,2)))
    assert(!LazyList(1,2).sameElements(LazyList(1)))
    assert(!LazyList(1).sameElements(LazyList(1,2)))
    assert(!LazyList(1).sameElements(LazyList(2)))
    assert(cycle1.sameElements(cycle1))
    assert(!cycle1.sameElements(cycle2))
    assert(!cycle1.sameElements(cycle2))
  }

  @Test
  def toStringIsStackSafe(): Unit = {
    val l = LazyList.from(Range.inclusive(1, 10000))
    l.foreach(_ => ())
    val s = l.toString // No exception thrown
  }

}
