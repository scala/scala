
package scala.concurrent

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.tools.testkit.AssertUtil._
import scala.util.{Success, Try}
import duration.Duration.Inf

class FutureTest {
  @Test
  def testZipWithFailFastBothWays(): Unit = {
    import ExecutionContext.Implicits.global

    val p1 = Promise[Int]()
    val p2 = Promise[Int]()

    // Make sure that the combined future fails early, after the earlier failure occurs, and does not
    // wait for the later failure regardless of which one is on the left and which is on the right
    p1.failure(new Exception("Boom Early"))
    val f1 = p1.future
    val f2 = p2.future

    val scala.util.Failure(fa) = Try(Await.result(f1.zip(f2), Inf))
    val scala.util.Failure(fb) = Try(Await.result(f2.zip(f1), Inf))

    val scala.util.Failure(fc) = Try(Await.result(f1.zipWith(f2)((_, _)), Inf))
    val scala.util.Failure(fd) = Try(Await.result(f2.zipWith(f1)((_, _)), Inf))

    val scala.util.Failure(fe) = Try(Await.result(Future.sequence(Seq(f1, f2)), Inf))
    val scala.util.Failure(ff) = Try(Await.result(Future.sequence(Seq(f2, f1)), Inf))

    val scala.util.Failure(fg) = Try(Await.result(Future.traverse(Seq(0, 1))(Seq(f1, f2)(_)), Inf))
    val scala.util.Failure(fh) = Try(Await.result(Future.traverse(Seq(0, 1))(Seq(f1, f2)(_)), Inf))

    // Make sure the early failure is always reported, regardless of whether it's on
    // the left or right of the zip/zipWith/sequence/traverse
    assert(fa.getMessage == "Boom Early")
    assert(fb.getMessage == "Boom Early")
    assert(fc.getMessage == "Boom Early")
    assert(fd.getMessage == "Boom Early")
    assert(fe.getMessage == "Boom Early")
    assert(ff.getMessage == "Boom Early")
    assert(fg.getMessage == "Boom Early")
    assert(fh.getMessage == "Boom Early")
  }

  @Test
  def `bug/issues#10513 firstCompletedOf must not leak references`(): Unit = {
    val unfulfilled = Promise[AnyRef]()
    val quick       = Promise[AnyRef]()
    val result      = new AnyRef
    // all callbacks will be registered
    val first = Future.firstCompletedOf(List(quick.future, unfulfilled.future))(ExecutionContext.parasitic)
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

  @Test
  def `bug/issues#9304 blocking shouldn't prevent Future from being resolved`(): Unit = {
    implicit val directExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(_.run())

    val p = Promise[Int]()
    val p0 = Promise[Int]()
    val p1 = Promise[Int]()

    val f = p0.future
      .flatMap { _ =>
        p.future
          .flatMap { _ =>
            val f = p0.future.flatMap { _ =>
              Future.successful(1)
            }
            // At this point scala.concurrent.Future.InternalCallbackExecutor has 1 runnable in _tasksLocal
            // (flatMap from the previous line)

            // blocking sets _tasksLocal to Nil (instead of null). Next it calls Batch.run, which checks
            // that _tasksLocal must be null, throws exception and all tasks are lost.
            // ... Because blocking throws an exception, we need to swallow it to demonstrate that Future `f` is not
            // completed.
            Try(blocking {
              1
            })

            f
          }
      }

    p.completeWith(p1.future.map(_ + 1))
    p0.complete(Success(0))
    p1.complete(Success(1))

    assertTrue(p.future.isCompleted)
    assertEquals(Some(Success(2)), p.future.value)

    assertTrue(f.isCompleted)
    assertEquals(Some(Success(1)), f.value)
  }
}
