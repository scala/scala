//> using jvm 9+
import scala.concurrent.{
  TimeoutException,
  ExecutionContext,
  ExecutionContextExecutorService,
  Await,
  Awaitable,
}
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.tools.testkit.AssertUtil.{Fast, Slow, waitFor, waitForIt}
import scala.util.{Try, Success, Failure}
import scala.util.chaining._
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit.{MILLISECONDS => Milliseconds, SECONDS => Seconds}

trait TestBase {

  trait Done { def apply(proof: => Boolean): Unit }

  def once(body: Done => Unit): Unit = {
    import java.util.concurrent.LinkedBlockingQueue
    val q = new LinkedBlockingQueue[Try[Boolean]]
    body(new Done {
      def apply(proof: => Boolean): Unit = q offer Try(proof)
    })
    var tried: Try[Boolean] = null
    def check = q.poll(5000L, Milliseconds).tap(tried = _) != null
    waitForIt(check, progress = Slow, label = "concurrent-tck")
    assert(tried.isSuccess)
    assert(tried.get)
    // Check that we don't get more than one completion
    assert(q.poll(50, Milliseconds) eq null)
  }

  def test[T](name: String)(body: => T): T = {
    println(s"starting $name")
    body.tap(_ => println(s"finished $name"))
  }

  def await[A](value: Awaitable[A]): A = {
    def check: Option[A] =
      Try(Await.result(value, Duration(500, "ms"))) match {
        case Success(x) => Some(x)
        case Failure(_: TimeoutException) => None
        case Failure(t) => throw t
      }
    waitFor(check, progress = Fast, label = "concurrent-tck test result")
  }
}

class ReportingExecutionContext extends TestBase {
  val progress = Fast
  @volatile var thread: Thread = null
  @volatile var reportedOn: Thread = null
  @volatile var reported: Throwable = null
  val latch = new CountDownLatch(1)

  def report(t: Thread, e: Throwable): Unit = {
    reportedOn = t
    reported = e
    latch.countDown()
  }

  def ecesUsingDefaultFactory = {
    import java.util.concurrent.{ForkJoinPool}
    import java.util.function.Predicate
    import scala.reflect.internal.util.RichClassLoader._

    val path = "java.util.concurrent.ForkJoinPool"
    val n = 2 // parallelism
    val factory = scala.concurrent.TestUtil.threadFactory(report)
    val ueh: Thread.UncaughtExceptionHandler = report(_, _)
    val async = true
    val coreSize = 4
    val maxSize = 4
    val minRun = 1 // minimumRunnable for liveness
    val saturate: Predicate[ForkJoinPool] = (fjp: ForkJoinPool) => false // whether to continue after blocking at maxSize
    val keepAlive = 2000L
    val fjp = new ForkJoinPool(n, factory, ueh, async, coreSize, maxSize, minRun, saturate, keepAlive, Milliseconds)
    ExecutionContext.fromExecutorService(fjp, report(null, _))
  }

  def testUncaughtExceptionReporting(ec: ExecutionContextExecutorService): Unit = once {
    done =>
    val example = new InterruptedException

    @tailrec def spinForThreadDeath(turns: Int): Boolean =
      turns > 0 && (thread != null && !thread.isAlive || { Thread.sleep(100L); spinForThreadDeath(turns - 1) })

    def truthfully(b: Boolean): Option[Boolean] = if (b) Some(true) else None

    // jdk17 thread receives pool exception handler, so wait for thread to die slow and painful expired keepalive
    def threadIsDead = waitFor(truthfully(spinForThreadDeath(turns = 10)), progress = progress, label = "concurrent-tck-thread-death")

    try {
      ec.execute(() => {
        thread = Thread.currentThread
        throw example
      })
      latch.await(2, Seconds)
      done(threadIsDead && (example.eq(reported) || example.eq(reported.getCause)))
    }
    finally ec.shutdown()
  }

  test("testUncaughtExceptionReporting")(testUncaughtExceptionReporting {
    ecesUsingDefaultFactory
  })
}

object Test extends App {
  new ReportingExecutionContext

  System.exit(0)
}

package scala.concurrent {
  object TestUtil {
    def threadFactory(uncaughtExceptionHandler: Thread.UncaughtExceptionHandler) = new impl.ExecutionContextImpl.DefaultThreadFactory(daemonic=true, maxBlockers=256, prefix="test-thread", uncaughtExceptionHandler)
  }
}
