package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.collection.Iterator
import scala.collection.mutable.ListBuffer
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

  /* GC tests */

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

  /* misc */

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

  /* laziness tests */

  /** Note: not reusable. */
  sealed abstract class LazinessChecker {
    import LazinessChecker._

    protected[this] val heads  = new Array[Boolean](count)
    protected[this] val states = new Array[Boolean](count)

    protected[this] def internalCheckIndex(index: Int): Unit = {
      assert(index >= 0 && index < count, "internal failure - bad index: " + index)
    }

    protected[this] def checkIndex(index: Int): Unit = {
      if (index < 0 || index >= count) throw new IndexOutOfBoundsException(index.toString)
    }

    final def stateEvaluated(index: Int): Boolean = {
      checkIndex(index)
      states(index)
    }

    final def headEvaluated(index: Int): Boolean = {
      checkIndex(index)
      heads(index)
    }

    private[this] def assertAll(arr: Array[Boolean], name: String, evaluated: Boolean, ex: String => Throwable): Unit =
      for (i <- 0 until count) {
        internalCheckIndex(i)
        if (arr(i) != evaluated) throw ex(s"$name($i) was ${if (evaluated) "not " else ""}evaluated")
      }

    final def assertAllStates(evaluated: Boolean, ex: String => Throwable = new AssertionError(_)): Unit =
      assertAll(states, "state", evaluated, ex)

    final def assertAllHeads(evaluated: Boolean, ex: String => Throwable = new AssertionError(_)): Unit =
      assertAll(heads, "head", evaluated, ex)

    // for debugging
    final override def toString: String = {
      val sb = new java.lang.StringBuilder(getClass.getSimpleName).append("(")
      for (i <- 0 until 4) { sb.append(s"state($i): ${states(i)}, head($i): ${heads(i)}, ") }
      sb.append("...)")
      sb.toString
    }
  }

  object LazinessChecker {
    final val count = 8
  }

  /* op laziness tests */

  /** Note: not reusable. */
  final class OpLazinessChecker extends LazinessChecker {
    import LazinessChecker._

    private[this] def gen(index: Int): LazyList[Int] = {
      def elem(): Int = {
        heads(index) = true
        index
      }
      def state(): LazyList[Int] =
        LazyList.unfold(0) { _ => { states(index) = true; None } }

      internalCheckIndex(index)
      state() #::: elem() #:: LazyList.empty[Int]
    }

    private[this] def genList(): LazyList[Int] = {
      def doGen(n: Int): LazyList[Int] =
        if (n < count) gen(n) #::: doGen(n + 1)
        else LazyList.unfold(0)(_ => None)

      doGen(0)
    }

    val lazyList: LazyList[Int] = genList()
  }

  object OpLazinessChecker {
    final case class DropProfile(dropCount: Int, repeatedDrops: Boolean) {
      def apply(iteration: Int): Int =
        if (iteration <= 0) 0
        else if (repeatedDrops) dropCount * iteration
        else dropCount
    }

    val NoDrops: DropProfile = DropProfile(dropCount = 0, repeatedDrops = false)
  }

  // save on having to write type annotations all the time
  def lazyListOp[U](op: (=> LazyList[Int]) => U): (=> LazyList[Int]) => U = op

  /** Asserts that the operation does not evaluate the the initial lazy list
    * at all before methods are invoked on it.
    */
  def assertInitiallyFullyLazy[U](op: (=> LazyList[Int]) => U): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    assert(!checker.stateEvaluated(0))
    assert(!checker.headEvaluated(0))
  }

  /** Asserts that the operation does not evaluate the initial lazy list or
    * subsequent tails at all before methods are invoked on them.
    *
    * This is a superset of the assertions performed by [[assertInitiallyFullyLazy]].
    */
  def assertRepeatedlyFullyLazy(op: (=> LazyList[Int]) => LazyList[Int],
                                d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    assert(!checker.stateEvaluated(0))
    assert(!checker.headEvaluated(0))
    result.head
    result.tail
    assert(!checker.stateEvaluated(1 + d(1)))
    assert(!checker.headEvaluated(1 + d(1)))
  }

  /** Asserts that the operation does not evaluate the tail of the lazy list
    * when the head is evaluated.
    */
  def assertLazyTailWhenHeadEvaluated(op: (=> LazyList[Int]) => LazyList[Int],
                                      d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.head
    assert(!checker.stateEvaluated(1 + d(1)))
    result.tail.head
    assert(!checker.stateEvaluated(2 + d(2)))
  }

  /** Asserts that the operation does not evaluate the head of the lazy list
    * when the tail is evaluated.
    */
  def assertLazyHeadWhenTailEvaluated(op: (=> LazyList[Int]) => LazyList[Int],
                                      d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.tail
    assert(!checker.headEvaluated(0 + d(1)))
    result.tail.tail
    assert(!checker.headEvaluated(1 + d(2)))
  }

  def assertLazyHeadWhenNextHeadEvaluated(op: (=> LazyList[Int]) => LazyList[Int],
                                          d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.tail.tail.head
    assert(!checker.headEvaluated(1 + d(2)))
    result.tail.head
    assert(!checker.headEvaluated(0 + d(1)))
  }

  /** Asserts that, though the operation may evaluate initial states and later
    * heads, it does not evaluate initial heads.
    */
  def assertLazyInitialHeads[U](op: (=> LazyList[Int]) => U): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    assert(!checker.headEvaluated(0))
    assert(!checker.headEvaluated(1))
  }

  /** Asserts that, though the operation may evaluate states and initial
    * heads, it does not evaluate later heads.
    */
  def assertLazyFinalHeads[U](op: (=> LazyList[Int]) => U): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    assert(!checker.headEvaluated(LazinessChecker.count - 1))
    assert(!checker.headEvaluated(LazinessChecker.count - 2))
  }

  /** Asserts that, though the operation may evaluate states, it does not
    * evaluate any heads.
    */
  def assertLazyAllHeads[U](op: (=> LazyList[Int]) => U): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    checker.assertAllHeads(evaluated = false)
  }

  // if this fails, all the rest will fail
  @Test
  def checker_correctness_properlyLazy(): Unit = {
    val checker = new OpLazinessChecker
    val illegalState = (s: String) => new IllegalStateException("sanity check failed: " + s)

    // check that none start evaluated
    checker.assertAllStates(evaluated = false, illegalState)
    checker.assertAllHeads(evaluated = false, illegalState)

    // check that it detects state evaluation
    checker.lazyList.isEmpty
    assert(checker.stateEvaluated(0))
    assert(!checker.headEvaluated(0))
    checker.lazyList.tail.isEmpty
    assert(!checker.headEvaluated(0))
    assert(checker.stateEvaluated(1))
    assert(!checker.headEvaluated(1))

    // check that all are evaluated after forcing
    checker.lazyList.force

    checker.assertAllStates(evaluated = true, illegalState)
    checker.assertAllHeads(evaluated = true, illegalState)

    // check unmodified checker is properly lazy
    val op = lazyListOp(x => x)
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def filter_properlyLazy(): Unit = {
    val op1 = lazyListOp(_.filter(_ => true))
    assertRepeatedlyFullyLazy(op1)
    assertLazyTailWhenHeadEvaluated(op1)

    val op2 = lazyListOp(_.filter(_ % 2 != 0))
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op2, d)
    assertLazyTailWhenHeadEvaluated(op2, d)
  }

  @Test
  def filterNot_properlyLazy(): Unit = {
    val op1 = lazyListOp(_.filterNot(_ => false))
    assertRepeatedlyFullyLazy(op1)
    assertLazyTailWhenHeadEvaluated(op1)

    val op2 = lazyListOp(_.filterNot(_ % 2 == 0))
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op2, d)
    assertLazyTailWhenHeadEvaluated(op2, d)
  }

  @Test
  def withFilter_properlyLazy(): Unit = {
    assertInitiallyFullyLazy(_.withFilter(_ => true))
    assertInitiallyFullyLazy(_.withFilter(_ % 2 != 0))
  }

  @Test
  def partition_properlyLazy(): Unit = {
    val partition = lazyListOp(_.partition(_ % 2 == 0))
    val op1 = partition.andThen(_._1)
    val op2 = partition.andThen(_._2)
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    for (op <- op1 :: op2 :: Nil) {
      assertRepeatedlyFullyLazy(op, d)
      assertLazyTailWhenHeadEvaluated(op, d)
    }
  }

  @Test
  def map_properlyLazy(): Unit = {
    val op = lazyListOp(_.map(_ + 1))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def tapEach_properlyLazy(): Unit = {
    val op = lazyListOp(_.tapEach(_ + 1))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def collect_properlyLazy(): Unit = {
    val op = lazyListOp(_ collect { case i if i % 2 != 0 => i})
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyTailWhenHeadEvaluated(op, d)
  }

  @Test // scala/scala#6960
  def withFilter_withFilter_properlyLazy(): Unit = {
    assertInitiallyFullyLazy(_.withFilter(_ => true).withFilter(_ => true))
  }

  @Test // scala/bug#9134
  def filter_map_properlyLazy(): Unit = {
    val op = lazyListOp(_.filter(_ % 2 != 0).map(identity))
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyTailWhenHeadEvaluated(op, d)
  }

  @Test // scala/bug#9134
  def withFilter_map_properlyLazy(): Unit = {
    val op = lazyListOp(_.withFilter(_ % 2 != 0).map(identity))
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyTailWhenHeadEvaluated(op, d)
  }

  @Test
  def flatMap_properlyLazy(): Unit = {
    val op = lazyListOp(_.flatMap(_ :: Nil))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)

    // Check that `flatMap`ing instances of `LazyList` leaves them with lazy heads
    val checkers = (1 to 4) map { _ => new OpLazinessChecker }
    LazyList.from(0)
      .take(checkers.length)
      .flatMap(i => checkers(i).lazyList)
    for (checker <- checkers) checker.assertAllHeads(evaluated = false)
  }

  @Test
  def scanLeft_properlyLazy(): Unit = {
    val op = lazyListOp(_.scanLeft(0)(_ + _))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
  }

  @Test
  def lazyAppendedAll_properlyLazy(): Unit = {
    def check(suffix: => IterableOnce[Int]): Unit = {
      val op = lazyListOp(_.lazyAppendedAll(suffix))
      assertRepeatedlyFullyLazy(op)
      assertLazyTailWhenHeadEvaluated(op)
      assertLazyHeadWhenTailEvaluated(op)
      assertLazyHeadWhenNextHeadEvaluated(op)
    }
    for (coll <- Seq(Nil, 2 :: Nil, 1 to 10)) check(coll)

    // Check that appending a `LazyList` leaves it fully lazy
    val checker = new OpLazinessChecker
    val ll = LazyList.from(0)
      .take(4)
      .lazyAppendedAll(checker.lazyList)
    checker.assertAllStates(evaluated = false)
    checker.assertAllHeads(evaluated = false)
    ll.tail.tail.tail.tail // should be the appended LazyList
    checker.assertAllStates(evaluated = false)
    checker.assertAllHeads(evaluated = false)
    ll.length // evaluate states
    checker.assertAllHeads(evaluated = false)
  }

  @Test
  def prepended_properlyLazy(): Unit = {
    val op = lazyListOp { ll =>
      val prepended = ll.prepended(-1)
      prepended.head
      prepended.tail
    }
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def prependedAll_properlyLazy(): Unit = {
    def check(prefix: IterableOnce[Int], drop: Int): Unit = {
      val op = lazyListOp { ll =>
        var prepended = ll.prependedAll(prefix)
        var toDrop = drop
        while (toDrop > 0) {
          prepended.head
          prepended = prepended.tail
          toDrop -= 1
        }
        prepended
      }
      assertRepeatedlyFullyLazy(op)
      assertLazyTailWhenHeadEvaluated(op)
      assertLazyHeadWhenTailEvaluated(op)
      assertLazyHeadWhenNextHeadEvaluated(op)
    }
    for (coll <- Seq(Nil, 2 :: Nil, 1 to 10)) check(coll, coll.length)

    // Check that prepending a `LazyList` leaves it fully lazy
    val checker = new OpLazinessChecker
    val ll = LazyList.from(0)
      .take(4)
      .prependedAll(checker.lazyList)
    checker.assertAllStates(evaluated = false)
    checker.assertAllHeads(evaluated = false)
    ll.length // evaluate states
    checker.assertAllHeads(evaluated = false)
  }

  @Test
  def drop_properlyLazy(): Unit = {
    val op = lazyListOp(_.drop(2))
    val d = OpLazinessChecker.DropProfile(dropCount = 2, repeatedDrops = false)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyTailWhenHeadEvaluated(op, d)
    assertLazyHeadWhenTailEvaluated(op, d)
    assertLazyHeadWhenNextHeadEvaluated(op, d)
    assertLazyInitialHeads(op.andThen(_.force))
  }

  @Test
  def dropWhile_properlyLazy(): Unit = {
    val op = lazyListOp(_.dropWhile(_ < 2))
    val d = OpLazinessChecker.DropProfile(dropCount = 2, repeatedDrops = false)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyTailWhenHeadEvaluated(op, d)
  }

  @Test
  def dropRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.dropRight(2))
    assertInitiallyFullyLazy(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.force))
  }

  @Test
  def take_properlyLazy(): Unit = {
    val op = lazyListOp(_.take(4))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.force))
  }

  @Test
  def takeWhile_properlyLazy(): Unit = {
    val op = lazyListOp(_.takeWhile(_ < 4))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.force))
  }

  @Test
  def takeRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.takeRight(4))
    assertInitiallyFullyLazy(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyInitialHeads(op.andThen(_.force))
  }

  @Test // scala/bug#11089
  def last_properlyLazy(): Unit = {
    assertLazyInitialHeads(_.last)
  }

  @Test
  def length_properlyLazy(): Unit = {
    assertLazyAllHeads(_.length)
  }

  @Test
  def lengthCompare_properlyLazy(): Unit = {
    assertLazyAllHeads(_.lengthCompare(LazinessChecker.count))
  }

  @Test
  def reverse_properlyLazy(): Unit = {
    assertLazyAllHeads(_.reverse)
  }

  @Test
  def toString_properlyLazy(): Unit = {
    val op = lazyListOp(_.toString())
    assertInitiallyFullyLazy(op)
    assertLazyAllHeads(op)
  }

  /* factory laziness tests */

  /** Note: not reusable.
    *
    * racy, but not being used in a concurrent environment
    */
  final class FactoryLazinessChecker extends LazinessChecker {
    private[this] var ll: LazyList[Int] = _

    final class Initializer private[FactoryLazinessChecker] {
      private[this] def evaluate(arr: Array[Boolean], name: String, index: Int): Unit = {
        checkIndex(index)
        if (arr(index)) throw new IllegalStateException(s"Can only evaluate $name($index) once")
        arr(index) = true
      }

      /** Marks state evaluated for a given index. */
      def evaluateState(index: Int): Unit = evaluate(states, "state", index)

      /** Marks head evaluated for a given index. */
      def evaluateHead(index: Int): Unit = evaluate(heads, "head", index)

      /** Marks state and head evaluated for a given index. */
      def evaluateIndex(index: Int): Unit = {
        evaluateState(index)
        evaluateHead(index)
      }
    }

    def initialize(init: Initializer => LazyList[Int]): this.type = {
      if (ll ne null) throw new IllegalStateException("already initialized")
      val res = init(new Initializer)
      if (res eq null) throw new NullPointerException("null LazyList")
      ll = res
      this
    }

    def lazyList: LazyList[Int] = {
      if (ll eq null) throw new IllegalStateException("not initialized")
      ll
    }
  }

  object FactoryLazinessChecker {
    type Factory = FactoryLazinessChecker#Initializer => LazyList[Int]
  }

  def lazyListFactory(factory: FactoryLazinessChecker.Factory): FactoryLazinessChecker.Factory = factory

  def assertLazyTailWhenHeadEvaluated(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.lazyList.head
    assert(!checker.stateEvaluated(1))
    checker.lazyList.tail.head
    assert(!checker.stateEvaluated(2))
  }

  def assertLazyHeadWhenTailEvaluated(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.lazyList.take(LazinessChecker.count).length // evaluate all tails
    checker.assertAllHeads(evaluated = false)
  }

  def assertLazyHeadWhenNextHeadEvaluated(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.lazyList.tail.tail.head
    assert(!checker.headEvaluated(1))
    checker.lazyList.tail.head
    assert(!checker.headEvaluated(0))
  }

  @Test
  def fromIterator_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.from(Iterator.tabulate(LazinessChecker.count) { i =>
        init.evaluateIndex(i)
        i
      })
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def fromLazyList_properlyLazy(): Unit = {
    val op = lazyListOp(ll => LazyList.from(ll))
    assertRepeatedlyFullyLazy(op)
    assertLazyTailWhenHeadEvaluated(op)
    assertLazyHeadWhenTailEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def unfold_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.unfold(0) { i =>
        if (i >= LazinessChecker.count) None
        else {
          init.evaluateIndex(i)
          Some(i, i + 1)
        }
      }
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def iterate_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.iterate(0) { i => init.evaluateIndex(i); i + 1 }
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def iterateLen_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.iterate(0, 10) { i => init.evaluateIndex(i); i + 1 }
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def tabulate_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.tabulate(LazinessChecker.count) { i => init.evaluateIndex(i); i }
    }
    assertLazyHeadWhenTailEvaluated(factory)
    assertLazyHeadWhenNextHeadEvaluated(factory)
  }

  @Test
  def `#:: and #::: properlyLazy`(): Unit = {
    val factory = lazyListFactory { init =>
      def gen(index: Int): LazyList[Int] = {
        def state(): LazyList[Int] =
          LazyList.unfold(0) { _ => init.evaluateState(index); None }
        def elem(): Int = {
          init.evaluateHead(index)
          index
        }
        if (index >= LazinessChecker.count) LazyList.empty
        else state() #::: elem() #:: gen(index + 1)
      }

      gen(0)
    }
    assertLazyTailWhenHeadEvaluated(factory)
    assertLazyHeadWhenTailEvaluated(factory)
    assertLazyHeadWhenNextHeadEvaluated(factory)
  }

  @Test
  def fill_properlyLazy(): Unit = {
    var counter = 0
    val lazyList = LazyList.fill(10) { counter += 1; counter }
    lazyList.length
    assertEquals(0, counter)
    assertEquals(1, lazyList(4))
    assertEquals(1, counter)
    assertEquals(2, lazyList.head)
    assertEquals(2, counter)
  }

  @Test
  def continually_properlyLazy(): Unit = {
    var counter = 0
    val lazyList = LazyList continually { counter += 1; counter }
    lazyList.lengthCompare(10) // evaluate first 10 states
    assertEquals(0, counter)
    assertEquals(1, lazyList(4))
    assertEquals(1, counter)
    assertEquals(2, lazyList.head)
    assertEquals(2, counter)
  }

  /* misc */

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
  @Test
  def tapEach: Unit = {

    /** @param makeLL must make a lazylist that evaluates to Seq(1,2,3,4,5) */
    def check(makeLL: => LazyList[Int]): Unit = {
      val lb = ListBuffer[Int]()
      val ll = makeLL.tapEach(lb += _)
      assertEquals(ListBuffer[Int](), lb)
      assertEquals(Vector(1, 2), ll.take(2).to(Vector))
      assertEquals(ListBuffer(1, 2), lb)
      assertEquals(4, ll(3))
      assertEquals(ListBuffer(1, 2, 4), lb)
      assertEquals(Vector(1,2,3,4,5), ll.to(Vector))
      assertEquals(ListBuffer(1, 2, 4, 3, 5), lb)
    }

    check(LazyList.from(Iterator(1, 2, 3, 4, 5)))
    check(LazyList.from(Vector(1, 2, 3, 4, 5)))
    check(LazyList.tabulate(5)(_ + 1))
  }

  @Test
  def testDistinctCircularLazyList : Unit = {
    lazy val ring : LazyList[Int] = 1 #:: 2 #:: 3 #:: 4 #:: 5#:: ring
    assertEquals(ring.distinct.toList, List(1,2,3,4,5))

    lazy val ring1 : LazyList[Int] = 1 #:: 2 #:: 3 #:: 4 #:: 5#:: ring1
    val distinctByFunc : Int => Int = {i : Int => if(i == 4) 3 else i}
    assertEquals(ring1.distinctBy(distinctByFunc), List(1,2,3,5))

    lazy val nonRing : LazyList[Int] = 1 #:: 2 #:: 3 #:: 4 #:: 5#:: LazyList(2,6) //added as part of regression to test non circular lazyList
    assertEquals(nonRing.distinct, List(1,2,3,4,5,6))
  }
}
