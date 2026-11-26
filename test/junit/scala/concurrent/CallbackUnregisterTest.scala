package scala.concurrent

import java.util.concurrent.TimeUnit.SECONDS
import java.util.concurrent.atomic.AtomicInteger

import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue, fail}
import org.junit.Test

import scala.concurrent.duration._
import scala.concurrent.impl.Promise.{DefaultPromise, Link, ManyCallbacks}
import scala.reflect.ClassTag
import scala.tools.testkit.AssertUtil.{assertNotReachable, assertThrows}
import scala.util.{Success, Try}

/** Removing a callback from a promise, which a timed-out or interrupted `Await` and `Future.firstCompletedOf` do.
 *
 *  The other callbacks of the promise must stay and run once, whether the removed callback is there or not. It is not
 *  there while `linkRootOf` (`flatMap`, `transformWith`, `recoverWith`) is moving it to the root: the inner promise is
 *  already a `Link`, and the root does not hold its callbacks yet. The tests put a promise into that state directly.
 */
class CallbackUnregisterTest {
  private implicit val ec: ExecutionContext = ExecutionContext.parasitic

  private def dp[T](f: Future[T]): DefaultPromise[T] = f.asInstanceOf[DefaultPromise[T]]

  private val Noop: AnyRef = new DefaultPromise[Any]().get()

  /** The callbacks of a future that is neither completed nor linked, newest first. */
  private def callbacks(f: Future[_]): List[AnyRef] = {
    def loop(cs: AnyRef): List[AnyRef] = cs match {
      case m: ManyCallbacks[_]    => m.first :: loop(m.rest)
      case Noop                   => Nil
      case _: Try[_] | _: Link[_] => throw new AssertionError(s"completed or linked: $cs")
      case t                      => t :: Nil
    }
    loop(dp(f).get())
  }

  /** Registers `n` callbacks on `f` and counts how often they run. */
  private def counting(f: Future[_], n: Int): AtomicInteger = {
    val ran = new AtomicInteger
    for (_ <- 1 to n) f.onComplete(_ => ran.incrementAndGet())
    ran
  }

  /** `gate.future.flatMap(_ => inner)`: completing `gate` links `inner` to the result. */
  private def flatMapOf(inner: Future[Int]): (Promise[Unit], Future[Int]) = {
    val gate = Promise[Unit]()
    (gate, gate.future.flatMap(_ => inner))
  }

  /** The first step of `linkRootOf`: `inner` becomes a `Link` to `root`, which does not have inner's callbacks yet. */
  private def halfLink(inner: Future[Int], root: Future[Int]): Unit =
    assertTrue(dp(inner).compareAndSet(dp(inner).get(), new Link(dp(root))))

  private def eventually(what: String)(cond: => Boolean): Unit = {
    val deadline = System.nanoTime() + SECONDS.toNanos(30)
    while (!cond) {
      if (System.nanoTime() > deadline) fail(s"timed out waiting for $what")
      Thread.`yield`()
    }
  }

  private final class Waiter(body: => Any) extends Thread {
    @volatile private var thrown: Throwable = null
    setDaemon(true)
    override def run(): Unit = try { body; () } catch { case t: Throwable => thrown = t }
    private def joined(): Throwable = {
      join(SECONDS.toMillis(30))
      assertFalse("thread is still running", isAlive)
      thrown
    }
    def finish(): Unit = joined() match {
      case null => ()
      case t    => throw new AssertionError(t)
    }
    def finishWith[E <: Throwable](implicit E: ClassTag[E]): Unit = {
      val t = joined()
      assertTrue(s"expected ${E.runtimeClass.getName}, got $t", E.runtimeClass.isInstance(t))
    }
  }

  /** Runs `await` on another thread and returns once its latch is registered on `on`, which must not be linked. */
  private def waiter(on: Future[Int])(await: => Any): Waiter = {
    val n = callbacks(on).size
    val w = new Waiter(await)
    w.start()
    eventually("the latch of Await")(!w.isAlive || callbacks(on).size > n)
    w
  }

  /** Ways to register a callback on a future. Each returns the function that removes it again.
   *
   *  A timed-out `Await` removes its latch as an interrupted one does, but at a time the test cannot choose.
   */
  private val removals: List[(String, Future[Int] => () => Unit)] = List(
    "onCompleteWithUnregister" -> { f =>
      dp(f).onCompleteWithUnregister(_ => ())
    },
    "firstCompletedOf" -> { f =>
      val other = Promise[Int]()
      val first = Future.firstCompletedOf(List(f, other.future))
      () => { other.success(0); assertEquals(Some(Success(0)), first.value) }
    },
    "Await.ready, interrupted" -> { f =>
      val w = waiter(f)(Await.ready(f, Duration.Inf))
      () => { w.interrupt(); w.finishWith[InterruptedException] }
    },
    "Await.result, interrupted" -> { f =>
      val w = waiter(f)(Await.result(f, 1.hour))
      () => { w.interrupt(); w.finishWith[InterruptedException] }
    },
  )

  /** Links `first` through `links` promises; the last link is real if `complete`, else only its first step is done.
   *  Returns the root, on which three callbacks were registered before linking, and their counter.
   */
  private def linked(first: Future[Int], links: Int, complete: Boolean): (Future[Int], AtomicInteger) = {
    var last = first
    for (_ <- 1 until links) {
      val (gate, next) = flatMapOf(last)
      gate.success(())
      last = next
    }
    val (gate, root) = flatMapOf(last)
    val ran = counting(root, 3)
    if (complete) gate.success(()) else halfLink(last, root)
    (root, ran)
  }

  @Test def unregisterAtEachPosition(): Unit =
    for (before <- 0 to 3; after <- 0 to 3) {
      val p = Promise[Int]()
      val what = s"$before before, $after after"
      val ranBefore = counting(p.future, before)
      val ranRemoved = new AtomicInteger
      val remove = dp(p.future).onCompleteWithUnregister(_ => ranRemoved.incrementAndGet())
      val removed = callbacks(p.future).head
      val ranAfter = counting(p.future, after)
      val others = callbacks(p.future).filterNot(_ eq removed)
      assertEquals(what, before + after, others.size)

      remove()
      assertEquals(what, others.toSet, callbacks(p.future).toSet)
      if (before + after == 0) assertSame(Noop, dp(p.future).get())

      p.success(1)
      assertEquals(what, before + after, ranBefore.get + ranAfter.get)
      assertEquals(what, 0, ranRemoved.get)
    }

  @Test def unregisterAbsent(): Unit =
    for (n <- 0 to 4) {
      val p = Promise[Int]()
      val remove = dp(p.future).onCompleteWithUnregister(_ => ())
      val ran = counting(p.future, n)
      remove()
      val state = dp(p.future).get()
      assertEquals(n, callbacks(p.future).size)

      remove() // no longer there
      assertSame(s"$n callbacks", state, dp(p.future).get())

      p.success(1)
      assertEquals(s"$n callbacks", n, ran.get)
    }

  @Test def unregisterFromCompleted(): Unit = {
    val p = Promise[Int]()
    val ran = counting(p.future, 2)
    val removedRan = new AtomicInteger
    val remove = dp(p.future).onCompleteWithUnregister(_ => removedRan.incrementAndGet())
    p.success(1)
    val state = dp(p.future).get()
    remove()
    remove()
    assertSame(state, dp(p.future).get())
    assertEquals(2, ran.get)
    assertEquals(1, removedRan.get)

    // the root of a link completes first: unregistering through the link finds the result, not callbacks
    for (complete <- List(true, false)) {
      val inner = Promise[Int]()
      val removeInner = dp(inner.future).onCompleteWithUnregister(_ => ())
      val (root, rootRan) = linked(inner.future, links = 1, complete)
      inner.success(2) // `inner` stays a link to the completed root
      removeInner()
      assertEquals(3, rootRan.get)
      assertEquals(Some(Success(2)), root.value)
      assertEquals(Some(Success(2)), inner.future.value)
    }
  }

  @Test def unregisterWhileLinking(): Unit =
    for ((name, register) <- removals; links <- 1 to 3) {
      val what = s"$name, $links link(s)"
      val first = Promise[Int]()
      val remove = register(first.future)
      val (root, ran) = linked(first.future, links, complete = false)
      val state = dp(root).get()

      remove() // follows the links to the root, where the callback is not registered yet
      assertSame(what, state, dp(root).get())

      first.success(1)
      assertEquals(what, Some(Success(1)), root.value)
      assertEquals(what, 3, ran.get)
    }

  @Test def unregisterAfterLink(): Unit =
    for ((name, register) <- removals; links <- 1 to 3) {
      val what = s"$name, $links link(s)"
      val first = Promise[Int]()
      val remove = register(first.future)
      val (root, ran) = linked(first.future, links, complete = true)

      remove()
      assertEquals(what, 3, callbacks(root).size)

      first.success(1)
      assertEquals(what, Some(Success(1)), root.value)
      assertEquals(what, 3, ran.get)
    }

  @Test def timedOutAwaitsOnLinkedFuture(): Unit = {
    val a = Promise[Int]()
    val (c, ran) = linked(a.future, links = 3, complete = true)
    val state = dp(c).get()

    for (_ <- 1 to 100) {
      assertThrows[TimeoutException](Await.ready(a.future, 1.nanosecond))
      assertThrows[TimeoutException](Await.result(a.future, 1.nanosecond))
      assertThrows[TimeoutException](Await.result(c, 1.nanosecond))
    }
    assertSame(state, dp(c).get())
    assertSame("the chain of links is compressed", dp(c), dp(a.future).get().asInstanceOf[Link[Int]].get())

    a.success(1)
    assertEquals(3, ran.get)
  }

  @Test def severalAwaitsWhileLinking(): Unit = {
    val inner = Promise[Int]()
    val waiters =
      List.fill(3)(waiter(inner.future)(Await.ready(inner.future, Duration.Inf))) ++
      List.fill(3)(waiter(inner.future)(Await.result(inner.future, 1.hour)))
    val (root, ran) = linked(inner.future, links = 1, complete = false)
    val state = dp(root).get()

    waiters.foreach(_.interrupt())
    waiters.foreach(_.finishWith[InterruptedException])
    assertSame(state, dp(root).get())

    inner.success(1)
    assertEquals(3, ran.get)
  }

  // Threads polling a linked future with short timeouts remove their latches from the root while others add callbacks
  @Test def severalTimedOutAwaitsConcurrently(): Unit = {
    val inner = Promise[Int]()
    val (root, ran) = linked(inner.future, links = 2, complete = true)
    val added = new AtomicInteger
    val pollers = List.tabulate(4)(i => new Waiter(
      for (_ <- 1 to 1000)
        assertThrows[TimeoutException](if (i % 2 == 0) Await.ready(inner.future, 1.nanosecond) else Await.result(inner.future, 1.nanosecond))))
    val adders = List.fill(2)(new Waiter(for (_ <- 1 to 500) root.onComplete(_ => added.incrementAndGet())))
    val threads = pollers ++ adders
    threads.foreach(_.start())
    threads.foreach(_.finish())
    assertEquals(1003, callbacks(root).size)

    inner.success(1)
    assertEquals(3, ran.get)
    assertEquals(1000, added.get)
  }

  // scala/bug#13197: an Await that gives up must not leave its latch on the future
  @Test def abandonedAwaitIsNotRetained(): Unit =
    for (links <- 0 to 2; timed <- List(true, false)) {
      val inner = Promise[Int]()
      inner.future.onComplete(_ => ())
      val w = waiter(inner.future)(if (timed) Await.result(inner.future, 1.hour) else Await.ready(inner.future, Duration.Inf))
      val latch = callbacks(inner.future).head
      val root = if (links == 0) inner.future else linked(inner.future, links, complete = true)._1
      assertTrue(callbacks(root).contains(latch))
      assertNotReachable(latch, inner, root) {
        w.interrupt()
        w.finishWith[InterruptedException]
      }
    }

  /* Races the removal of a callback of `inner` with the linking of `inner` to `root`. `inner` has many callbacks, which
   * makes the move to the root slow, so the removal often arrives in between the two steps of `linkRootOf`.
   */
  @Test def unregisterRacingWithLink(): Unit = {
    val rounds = 3000
    val go = new AtomicInteger
    val done = new AtomicInteger
    @volatile var link: Runnable = null
    @volatile var remove: Runnable = null
    @volatile var stop = false
    def racer(task: => Runnable) = new Waiter({
      var round = 1
      while (round <= rounds && !stop) {
        while (go.get < round && !stop) Thread.`yield`()
        if (!stop) {
          task.run()
          done.incrementAndGet()
          round += 1
        }
      }
    })
    val racers = List(racer(link), racer(remove))
    racers.foreach(_.start())

    try for (round <- 1 to rounds) {
      val inner = Promise[Int]()
      for (_ <- 1 to 500) inner.future.onComplete(_ => ())
      val unregister = dp(inner.future).onCompleteWithUnregister(_ => ())
      val (gate, root) = flatMapOf(inner.future)
      val ran = counting(root, 3)
      link = () => { gate.success(()); () }
      remove = () => unregister()

      go.set(round)
      eventually(s"round $round")(done.get == 2 * round)

      inner.success(1)
      assertEquals(Some(Success(1)), root.value)
      assertEquals(s"callbacks of the flatMap result that ran, round $round", 3, ran.get)
    } finally stop = true
    racers.foreach(_.finish())
  }
}
