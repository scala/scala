
package scala.concurrent

import java.util.concurrent.atomic.AtomicInteger

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._
import scala.util.Try


@RunWith(classOf[JUnit4])
class FutureTest {
  @Test
  def `bug/issues#10513 firstCompletedOf must not leak references`(): Unit = {
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
}
