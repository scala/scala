package scala.concurrent.impl

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.CountDownLatch
import org.junit.Assert._
import org.junit.{ After, Before, Test }
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.impl.Promise.DefaultPromise
import scala.util.{ Failure, Success, Try }
import scala.util.control.NonFatal

/** Tests for the private class DefaultPromise */
@RunWith(classOf[JUnit4])
class DefaultPromiseTest {

  // Many tests in this class use a helper class, Tester, to track the state of
  // promises and to ensure they behave correctly, particularly the complex behaviour
  // of linking.

  type Result = Int
  type PromiseId = Int
  type HandlerId = Int
  type ChainId = Int

  /** The state of a set of set of linked promises. */
  case class Chain(
    promises: Set[PromiseId],
    state: Either[Set[HandlerId],Try[Result]]
  )

  /** A helper class that provides methods for creating, linking, completing and
   *  adding handlers to promises. With each operation it verifies that handlers
   *  are called, any expected exceptions are thrown, and that all promises have
   *  the expected value.
   *
   *  The links between promises are not tracked precisely. Instead, linked promises
   *  are placed in the same Chain object. Each link in the same chain will share
   *  the same value.
   */
  class Tester {
    var promises = Map.empty[PromiseId, DefaultPromise[Result]]
    var chains = Map.empty[ChainId, Chain]

    private var counter = 0
    private def freshId(): Int = {
      val id = counter
      counter += 1
      id
    }

    /** Handlers report their activity on this queue */
    private val handlerQueue = new ConcurrentLinkedQueue[(Try[Result], HandlerId)]()

    /** Get the chain for a given promise */
    private def promiseChain(p: PromiseId): Option[(ChainId, Chain)] = {
      val found: Iterable[(ChainId, Chain)] = for ((cid, c) <- chains; p0 <- c.promises; if (p0 == p)) yield ((cid, c))
      found.toList match {
        case Nil => None
        case x::Nil => Some(x)
        case _ => throw new IllegalStateException(s"Promise $p found in more than one chain")
      }
    }

    /** Passed to `checkEffect` to indicate the expected effect of an operation */
    sealed trait Effect
    case object NoEffect extends Effect
    case class HandlersFired(result: Try[Result], handlers: Set[HandlerId]) extends Effect
    case object MaybeIllegalThrown extends Effect
    case object IllegalThrown extends Effect

    /** Runs an operation while verifying that the operation has the expected effect */
    private def checkEffect(expected: Effect)(f: => Any) {
      assert(handlerQueue.isEmpty()) // Should have been cleared by last usage
      val result = Try(f)

      var fireCounts = Map.empty[(Try[Result], HandlerId), Int]
      while (!handlerQueue.isEmpty()) {
        val key = handlerQueue.poll()
        val newCount = fireCounts.getOrElse(key, 0) + 1
        fireCounts = fireCounts.updated(key, newCount)
      }

      def assertIllegalResult = result match {
        case Failure(e: IllegalStateException) => ()
        case _ => fail(s"Expected IllegalStateException: $result")
      }

      expected match {
        case NoEffect =>
          assertTrue(s"Shouldn't throw exception: $result", result.isSuccess)
          assertEquals(Map.empty[(Try[Result], HandlerId), Int], fireCounts)
        case HandlersFired(firingResult, handlers) =>
          assert(result.isSuccess)
          val expectedCounts = handlers.foldLeft(Map.empty[(Try[Result], HandlerId), Int]) {
            case (map, hid) => map.updated((firingResult, hid), 1)
          }
          assertEquals(expectedCounts, fireCounts)
        case MaybeIllegalThrown =>
          if (result.isFailure) assertIllegalResult
          assertEquals(Map.empty, fireCounts)
        case IllegalThrown =>
          assertIllegalResult
          assertEquals(Map.empty, fireCounts)
      }
    }

    /** Check each promise has the expected value. */
    private def assertPromiseValues() {
      for ((cid, chain) <- chains; p <- chain.promises) {
        chain.state match {
          case Right(result) => assertEquals(Some(result), promises(p).value)
          case Left(_) => ()
        }
      }
    }

    /** Create a promise, returning a handle. */
    def newPromise(): PromiseId = {
      val pid = freshId()
      val cid = freshId()
      promises = promises.updated(pid, new DefaultPromise[Result]())
      chains = chains.updated(cid, Chain(Set(pid), Left(Set.empty)))
      assertPromiseValues()
      pid
    }

    /** Complete a promise */
    def complete(p: PromiseId) {
      val r = Success(freshId())
      val (cid, chain) = promiseChain(p).get
      val (completionEffect, newState) = chain.state match {
        case Left(handlers) => (HandlersFired(r, handlers), Right(r))
        case Right(completion) => (IllegalThrown, chain.state)
      }
      checkEffect(completionEffect) { promises(p).complete(r) }
      chains = chains.updated(cid, chain.copy(state = newState))
      assertPromiseValues()
    }

    /** Attempt to link two promises together */
    def link(a: PromiseId, b: PromiseId): (ChainId, ChainId) = {
      val promiseA = promises(a)
      val promiseB = promises(b)
      val (cidA, chainA) = promiseChain(a).get
      val (cidB, chainB) = promiseChain(b).get

      // Examine the state of each promise's chain to work out
      // the effect of linking the promises, and to work out
      // if the two chains should be merged.

      sealed trait MergeOp
      case object NoMerge extends MergeOp
      case class Merge(state: Either[Set[HandlerId],Try[Result]]) extends MergeOp

      val (linkEffect, mergeOp) = (chainA.state, chainB.state) match {
        case (Left(handlers1), Left(handlers2)) =>
          (NoEffect, Merge(Left(handlers1 ++ handlers2)))
        case (Left(handlers), Right(result)) =>
          (HandlersFired(result, handlers), Merge(Right(result)))
        case (Right(result), Left(handlers)) =>
          (HandlersFired(result, handlers), Merge(Right(result)))
        case (Right(_), Right(_)) if (cidA == cidB) =>
          (MaybeIllegalThrown, NoMerge) // Won't be thrown if happen to link a promise to itself
        case (Right(_), Right(_)) =>
          (IllegalThrown, NoMerge)
      }

      // Perform the linking and merge the chains, if appropriate

      checkEffect(linkEffect) { promiseA.linkRootOf(promiseB) }

      val (newCidA, newCidB) = mergeOp match {
        case NoMerge => (cidA, cidB)
        case Merge(newState) => {
          chains = chains - cidA
          chains = chains - cidB
          val newCid = freshId()
          chains = chains.updated(newCid, Chain(chainA.promises ++ chainB.promises, newState))
          (newCid, newCid)
        }
      }
      assertPromiseValues()
      (newCidA, newCidB)
    }

    /** Attach an onComplete handler. When called, the handler will
     *  place an entry into `handlerQueue` with the handler's identity.
     *  This allows verification of handler calling semantics.
     */
    def attachHandler(p: PromiseId): HandlerId = {
      val hid = freshId()
      val promise = promises(p)
      val (cid, chain) = promiseChain(p).get
      val (attachEffect, newState) = chain.state match {
        case Left(handlers) =>
          (NoEffect, Left(handlers + hid))
        case Right(result) =>
          (HandlersFired(result, Set(hid)), Right(result))
      }
      implicit val ec = new ExecutionContext {
        def execute(r: Runnable) { r.run() }
        def reportFailure(t: Throwable) { t.printStackTrace() }
      }

      checkEffect(attachEffect) { promise.onComplete(result => handlerQueue.add((result, hid))) }
      chains = chains.updated(cid, chain.copy(state = newState))
      assertPromiseValues()
      hid
    }
  }

  // Some methods and objects that build a list of promise
  // actions to test and then execute them

  type PromiseKey = Int

  sealed trait Action
  case class Complete(p: PromiseKey) extends Action
  case class Link(a: PromiseKey, b: PromiseKey) extends Action
  case class AttachHandler(p: PromiseKey) extends Action

  /** Tests a sequence of actions on a Tester. Creates promises as needed. */
  private def testActions(actions: Seq[Action]) {
    val t = new Tester()
    var pMap = Map.empty[PromiseKey, PromiseId]
    def byKey(key: PromiseKey): PromiseId = {
      if (!pMap.contains(key)) {
        pMap = pMap.updated(key, t.newPromise())
      }
      pMap(key)
    }

    actions foreach { action =>
      action match {
        case Complete(p) => t.complete(byKey(p))
        case Link(a, b) => t.link(byKey(a), byKey(b))
        case AttachHandler(p) => t.attachHandler(byKey(p))
      }
    }
  }

  /** Tests all permutations of actions for `count` promises */
  private def testPermutations(count: Int) {
    val ps = (0 until count).toList
    val pPairs = for (a <- ps; b <- ps) yield (a, b)

    var allActions = ps.map(Complete(_)) ++ pPairs.map { case (a, b) => Link(a, b) } ++ ps.map(AttachHandler(_))
    for ((permutation, i) <- allActions.permutations.zipWithIndex) {
      testActions(permutation)
    }
  }

  /** Test all permutations of actions with a single promise */
  @Test
  def testPermutations1 {
    testPermutations(1)
  }

  /** Test all permutations of actions with two promises - about 40 thousand */
  @Test
  def testPermutations2 {
    testPermutations(2)
  }

  /** Link promises in different orders, using the same link structure as is
   *  used in Future.flatMap */
  @Test
  def simulateFlatMapLinking {
    val random = new scala.util.Random(1)
    for (_ <- 0 until 10) {
      val t = new Tester()
      val flatMapCount = 100

      sealed trait FlatMapEvent
      case class Link(a: PromiseId, b: PromiseId) extends FlatMapEvent
      case class Complete(p: PromiseId) extends FlatMapEvent

      @tailrec
      def flatMapEvents(count: Int, p1: PromiseId, acc: List[FlatMapEvent]): List[FlatMapEvent] = {
        if (count == 0) {
          Complete(p1)::acc
        } else {
          val p2 = t.newPromise()
          flatMapEvents(count - 1, p2, Link(p2, p1)::acc)
        }
      }

      val events = flatMapEvents(flatMapCount, t.newPromise(), Nil)
      assertEquals(flatMapCount + 1, t.chains.size) // All promises are unlinked
      val shuffled = random.shuffle(events)
      shuffled foreach {
        case Link(a, b) => t.link(a, b)
        case Complete(p) => t.complete(p)
      }
      // All promises should be linked together, no matter the order of their linking
      assertEquals(1, t.chains.size)
    }
  }

  /** Link promises together on more than one thread, using the same link
   *  structure as is used in Future.flatMap */
  @Test
  def testFlatMapLinking {
    for (_ <- 0 until 100) {
      val flatMapCount = 100
      val startLatch = new CountDownLatch(1)
      val doneLatch = new CountDownLatch(flatMapCount + 1)
      def execute(f: => Unit) {
        val ec = ExecutionContext.global
        ec.execute(new Runnable {
          def run() {
            try {
              startLatch.await()
              f
              doneLatch.countDown()
            } catch {
              case NonFatal(e) => ec.reportFailure(e)
            }
          }
        })
      }
      @tailrec
      def flatMapTimes(count: Int, p1: DefaultPromise[Int]) {
        if (count == 0) {
          execute { p1.success(1) }
        } else {
          val p2 = new DefaultPromise[Int]()
          execute { p2.linkRootOf(p1) }
          flatMapTimes(count - 1, p2)
        }
      }

      val p = new DefaultPromise[Int]()
      flatMapTimes(flatMapCount, p)
      startLatch.countDown()
      doneLatch.await()
      assertEquals(Some(Success(1)), p.value)
    }
  }

}
