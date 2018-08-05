package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.collection.Iterator
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

    assertEquals(true, Try { wf.map(identity).length }.isFailure) // throws on n == 5

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
    assertLazyListOpLazyInTail(_.filter(_ % 2 == 0).map(identity), List(1))
  }

  @Test // scala/bug#9134
  def withFilter_map_properly_lazy_in_tail: Unit = {
    assertLazyListOpLazyInTail(_.withFilter(_ % 2 == 0).map(identity), List(1))
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
    assertEquals("LazyList()", LazyList.empty.force.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailBothAreNotEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    assertEquals("LazyList(?)", l.toString)
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
    assertEquals("LazyList(1, ?)", l.toString)
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
    assertEquals("LazyList(_, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadIsNotEvaluatedAndTailHeadIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.head
    assertEquals("LazyList(_, 2, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadIsNotEvaluatedAndTailTailIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.tail
    assertEquals("LazyList(_, _, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhendHeadIsNotEvaluatedAndTailTailHeadIsEvaluated = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.tail.head
    assertEquals("LazyList(_, _, 3, ?)", l.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListIsForcedToList: Unit = {
    val l = 1 #:: 2 #:: 3 #:: 4 #:: LazyList.empty
    l.toList
    assertEquals("LazyList(1, 2, 3, 4)", l.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListIsEmpty: Unit = {
    // cached empty
    val l1 = LazyList.empty
    assertEquals("LazyList()", l1.toString)
    // non-cached empty
    val l2 = LazyList.unfold(0)(_ => None)
    assertEquals("LazyList(?)", l2.toString)
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
    assertEquals("LazyList(?)", cyc.toString)
    cyc.head
    assertEquals("LazyList(1, ?)", cyc.toString)
    cyc.tail
    assertEquals("LazyList(1, ?)", cyc.toString)
    cyc.tail.head
    assertEquals("LazyList(1, 2, ?)", cyc.toString)
    cyc.tail.tail.head
    assertEquals("LazyList(1, 2, 3, ?)", cyc.toString)
    cyc.tail.tail.tail.head
    assertEquals("LazyList(1, 2, 3, 4, ...)", cyc.toString)
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

  @Test
  def laziness(): Unit = {
    lazy val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
    assert(List(0, 1, 1, 2) == fibs.take(4).to(List))

    var lazeCount = 0
    def lazeL(i: Int) = { lazeCount += 1; i }
    val xs21 = lazeL(1) #:: lazeL(2) #:: lazeL(3) #:: LazyList.empty

    assertEquals(0, lazeCount)
  }

  @Test  // Strawman issue #529
  def testLazyListMustComputeHeadOnlyOnce(): Unit = {
    var seedCounter = 0
    var fCounter = 0
    def seed(): Int = {
      seedCounter += 1
      1
    }
    val f: Int => Int = { x =>
      fCounter += 1
      x + 1
    }
    val xs = LazyList.iterate(seed())(f)
    assertEquals(0, seedCounter)
    assertEquals(0, fCounter)

    xs.head
    assertEquals(1, seedCounter)
    assertEquals(0, fCounter)

    xs.tail
    assertEquals(1, seedCounter)
    assertEquals(0, fCounter)

    xs.tail.head
    assertEquals(1, seedCounter)
    assertEquals(1, fCounter)

    xs.tail.tail
    assertEquals(1, seedCounter)
    assertEquals(1, fCounter)

    xs.tail.tail.head
    assertEquals(1, seedCounter)
    assertEquals(2, fCounter)

    xs.take(10).toList
    assertEquals(1, seedCounter)
    assertEquals(9, fCounter)
  }

  @Test
  def t8680: Unit = {
    def pre(n: Int) = (-n to -1).to(Stream)

    def cyc(m: Int) = {
      lazy val s: Stream[Int] = (0 until m).to(Stream) #::: s
      s
    }

    def precyc(n: Int, m: Int) = pre(n) #::: cyc(m)

    def goal(n: Int, m: Int) = (-n until m).mkString + "..."

    // Check un-forced cyclic and non-cyclic streams
    assertEquals("Stream(-2, ?)", pre(2).toString)
    assertEquals("Stream(0, ?)", cyc(2).toString)
    assertEquals("Stream(-2, ?)", precyc(2,2).toString)

    // Check forced cyclic and non-cyclic streams
    assertEquals("Stream(-2, -1)", pre(2).force.toString)
    assertEquals("Stream(0, 1, ...)", cyc(2).force.toString)
    assertEquals("Stream(-2, -1, 0, 1, ...)", precyc(2,2).force.toString)

    // Special cases
    assertEquals("Stream(0, ...)", cyc(1).force.toString)
    assertEquals("Stream(-1, 0, 1, 2, 3, 4, 5, ...)", precyc(1,6).force.toString)
    assertEquals("Stream(-6, -5, -4, -3, -2, -1, 0, ...)", precyc(6,1).force.toString)

    // Make sure there are no odd/even problems
    for (n <- 3 to 4; m <- 3 to 4) {
      assertEquals(s"mkString $n $m", precyc(n,m).mkString, goal(n,m))
    }

    // Make sure there are no cycle/prefix modulus problems
    for (i <- 6 to 8) {
      assertEquals(s"mkString $i 3", goal(i,3), precyc(i,3).mkString)
      assertEquals(s"mkString 3 $i", goal(3,i), precyc(3,i).mkString)
    }
  }
}
