
package scala.concurrent

import java.util.concurrent.atomic.AtomicInteger

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.tools.testing.AssertUtil._
import scala.util.{Success, Try}

class FutureTest {
  @Test
  def `scala/bug#10513 firstCompletedOf must not leak references`: Unit = {
    val unfulfilled = Promise[AnyRef]()
    val quick       = Promise[AnyRef]()
    val result      = new AnyRef
    // all callbacks will be registered
    val first = Future.firstCompletedOf(List(quick.future, unfulfilled.future))(Future.InternalCallbackExecutor)
    // callbacks run parasitically to avoid race or waiting for first future;
    // normally we have no guarantee that firstCompletedOf completed, so we assert that this assumption held
    assertNotReachable(result, unfulfilled) {
      quick.complete(Try(result))
      assertTrue("First must complete", first.isCompleted)
    }
    /* The test has this structure under the hood:
    val p = Promise[String]
    val q = Promise[String]
    val res = Promise[String]
    val s = "hi"
    p.future.onComplete(t => res.complete(t))
    q.future.onComplete(t => res.complete(t))   // previously, uncompleted promise held reference to promise completed with value
    assertNotReachable(s, q) {
      p.complete(Try(s))
    }
    */
  }

  // refactor for scala/bug#9304
  @Test def invariantsOfBatchingExecutor(): Unit = {
    val count = new AtomicInteger
    val be = new BatchingExecutor {
      def unbatchedExecute(r: Runnable): Unit = {
        r.run()
      }
      protected def resubmitOnBlock: Boolean = false
    }
    def test(): Unit = {
      val n = count.incrementAndGet()
      assertEquals(1, n)
    }
    be.execute(() => test())
    assertEquals(1, count.get)
  }

  @Test def `scala/bug#9304 batching executor failure when blocking`: Unit = {
    implicit val directExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(_.run())

    val p = Promise[Int]()
    val p0 = Promise[Int]()
    val p1 = Promise[Int]()

    val f =
      p0.future.flatMap { _ =>
        val g = p.future.flatMap { _ =>
          Future.successful(42)
        }
        // At this point scala.concurrent.Future.InternalCallbackExecutor has 1 runnable in _tasksLocal
        // (flatMap from the previous line)

        // blocking sets _tasksLocal to Nil (instead of null). Next it calls Batch.run, which checks
        // that _tasksLocal must be null, throws exception and all tasks are lost.
        // ... Because blocking throws an exception, we need to swallow it to demonstrate that Future `f` is not
        // completed.
        Try(blocking { 27 })
        g
      }

    p.completeWith(p1.future.map(_ + 1))
    p0.complete(Success(17))
    p1.complete(Success(5))

    assertTrue("p.future.isCompleted", p.future.isCompleted)
    assertEquals(Some(Success(5+1)), p.future.value)
    assertTrue("f.isCompleted", f.isCompleted)
    assertEquals(Some(Success(42)), f.value)
  }
}
