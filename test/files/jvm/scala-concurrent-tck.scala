
import scala.concurrent.{
  Future,
  Promise,
  TimeoutException,
  ExecutionException,
  ExecutionContext,
  ExecutionContextExecutorService,
  CanAwait,
  Await,
  Awaitable,
  blocking
}
import scala.annotation.{nowarn, tailrec}
import scala.concurrent.duration._
import scala.reflect.{classTag, ClassTag}
import scala.tools.testkit.AssertUtil.{Fast, Slow, assertThrows, waitFor, waitForIt}
import scala.util.{Try, Success, Failure}
import scala.util.chaining._
import java.util.concurrent.{CountDownLatch, ThreadPoolExecutor}
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


class FutureCallbacks extends TestBase {
  import ExecutionContext.Implicits._

  def testOnSuccess(): Unit = once {
    done =>
      var x = 0
      val f = Future { x = 1 }
      f foreach { _ => done(x == 1) }
  }

  def testOnSuccessWhenCompleted(): Unit = once {
    done =>
      var x = 0
      val f = Future { x = 1 }
      f onComplete {
        case Success(_) if x == 1 =>
          x = 2
          f foreach {  _ => done(x == 2) }
        case _ =>
      }
  }

  def testOnSuccessWhenFailed(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception }
      f onComplete {
        case Success(_) => done(false)
        case Failure(_) => done(true)
      }
  }

  def testOnFailure(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception }
      f onComplete {
        case Success(_) => done(false)
        case Failure(_) => done(true)
      }
  }

  def testOnFailureWhenSpecialThrowable(num: Int, cause: Throwable): Unit = once {
    done =>
      val f = Future[Unit] { throw cause }
      f onComplete {
        case Failure(e: ExecutionException) if e.getCause == cause => done(true)
        case _ => done(false)
      }
  }

  def testOnFailureWhenTimeoutException(): Unit = once {
    done =>
      val f = Future[Unit] { throw new TimeoutException() }
      f onComplete {
        case Success(_) => done(false)
        case Failure(e: TimeoutException) => done(true)
        case Failure(_) => done(false)
      }
  }

  def testThatNestedCallbacksDoNotYieldStackOverflow(): Unit = {
    val promise = Promise[Int]()
    (0 to 10000).map(Future(_)).foldLeft(promise.future)((pf, f) => f.flatMap(i => pf))
    promise.success(-1)
  }

  def stressTestNumberofCallbacks(): Unit = once {
    done =>
      val promise = Promise[Unit]()
      val otherPromise = Promise[Unit]()
      def attachMeaninglessCallbacksTo[T](f: Future[T]): Future[T] = {
        (1 to 20000).foreach(_ => f.onComplete(_ => ()))
        f
      }
      val future = attachMeaninglessCallbacksTo(promise.future).flatMap { _ =>
        attachMeaninglessCallbacksTo(otherPromise.future)
      }
      val numbers = new java.util.concurrent.ConcurrentHashMap[Int, Unit]()
      (0 to 10000) foreach { x => numbers.put(x, ()) }
      Future.sequence((0 to 10000) map { x => future.andThen({ case _ => numbers.remove(x) }) }) onComplete {
        _ => done(numbers.isEmpty)
      }
      promise.success(())
      otherPromise.success(())
  }

  test("testOnSuccess")(testOnSuccess())
  test("testOnSuccessWhenCompleted")(testOnSuccessWhenCompleted())
  test("testOnSuccessWhenFailed")(testOnSuccessWhenFailed())
  test("testOnFailure")(testOnFailure())
  test("testOnFailureWhenSpecialThrowable")(testOnFailureWhenSpecialThrowable(5, new Error))
  // testOnFailureWhenSpecialThrowable(6, new scala.util.control.ControlThrowable { })
  //TODO: this test is currently problematic, because NonFatal does not catch InterruptedException
  //testOnFailureWhenSpecialThrowable(7, new InterruptedException)
  test("testThatNestedCallbacksDoNotYieldStackOverflow")(testThatNestedCallbacksDoNotYieldStackOverflow())
  test("testOnFailureWhenTimeoutException")(testOnFailureWhenTimeoutException())
  test("stressTestNumberofCallbacks")(stressTestNumberofCallbacks())
}


class FutureCombinators extends TestBase {
  import ExecutionContext.Implicits._

  def testMapSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f map { x => "result: " + x }
      g onComplete {
        case Success(s) => done(s == "result: 5")
        case Failure(_) => done(false)
      }
  }

  def testMapFailure(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception("exception message") }
      val g = f map { x => "result: " + x }
      g onComplete {
        case Success(_) => done(false)
        case Failure(t) => done(t.getMessage() == "exception message")
      }
  }

  def testMapSuccessPF(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f map { case r => "result: " + r }
      g onComplete {
        case Success(s) => done(s == "result: 5")
        case Failure(_) => done(false)
      }
  }

  def testTransformSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f.transform(r => "result: " + r, identity)
      g onComplete {
        case Success(s) => done(s == "result: 5")
        case Failure(_) => done(false)
      }
  }

  def testTransformSuccessPF(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f.transform( { case r => "result: " + r }, identity)
      g onComplete {
        case Success(s) => done(s == "result: 5")
        case Failure(_) => done(false)
      }
  }

def testTransformFailure(): Unit = once {
    done =>
      val transformed = new Exception("transformed")
      val f = Future { throw new Exception("expected") }
      val g = f.transform(identity, _ => transformed)
      g onComplete {
        case Success(_) => done(false)
        case Failure(e) => done(e eq transformed)
      }
  }

  def testTransformFailurePF(): Unit = once {
    done =>
      val e = new Exception("expected")
      val transformed = new Exception("transformed")
      val f = Future[Unit] { throw e }
      val g = f.transform(identity, (_: Throwable @unchecked) match { case `e` => transformed })
      g onComplete {
        case Success(_) => done(false)
        case Failure(e) => done(e eq transformed)
      }
  }

  def testTransformResultToResult(): Unit = once {
    done =>
      Future("foo").transform {
        case Success(s) => Success(s.toUpperCase)
        case Failure(f) => throw new Exception("test failed")
      } onComplete {
        case Success("FOO") => done(true)
        case _ => done(false)
      }
  }

  def testTransformResultToFailure(): Unit = once {
    done => 
      val e = new Exception("expected")
      Future("foo").transform {
        case Success(s) => Failure(e)
        case Failure(f) => throw new Exception("test failed")
      } onComplete {
        case Failure(`e`) => done(true)
        case _ => done(false)
      }
  }

  def testTransformFailureToResult(): Unit = once {
    done =>
    val e = "foo"
      Future(throw new Exception("initial")).transform {
        case Success(s) => throw new Exception("test failed")
        case Failure(f) => Success(e)
      } onComplete {
        case Success(`e`) => done(true)
        case _ => done(false)
      }
  }

  def testTransformFailureToFailure(): Unit = once {
    done =>
      val e = new Exception("expected")
      Future(throw new Exception("initial")).transform {
        case Success(s) => throw new Exception("test failed")
        case Failure(f) => Failure(e)
      } onComplete {
        case Failure(`e`) => done(true)
        case _ => done(false)
      }
  }

    def testTransformWithResultToResult(): Unit = once {
    done =>
      Future("foo").transformWith {
        case Success(s) => Future(s.toUpperCase)
        case Failure(f) => throw new Exception("test failed")
      } onComplete {
        case Success("FOO") => done(true)
        case _ => done(false)
      }
  }

  def testTransformWithResultToFailure(): Unit = once {
    done => 
      val e = new Exception("expected")
      Future("foo").transformWith {
        case Success(s) => Future(throw e)
        case Failure(f) => throw new Exception("test failed")
      } onComplete {
        case Failure(`e`) => done(true)
        case _ => done(false)
      }
  }

  def testTransformWithFailureToResult(): Unit = once {
    done =>
    val e = "foo"
      Future(throw new Exception("initial")).transformWith {
        case Success(s) => throw new Exception("test failed")
        case Failure(f) => Future(e)
      } onComplete {
        case Success(`e`) => done(true)
        case _ => done(false)
      }
  }

  def testTransformWithFailureToFailure(): Unit = once {
    done =>
      val e = new Exception("expected")
      Future(throw new Exception("initial")).transformWith {
        case Success(s) => throw new Exception("test failed")
        case Failure(f) => Future(throw e)
      } onComplete {
        case Failure(`e`) => done(true)
        case _ => done(false)
      }
  }

  def testFoldFailure(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception("expected") }
      val g = f.transform(r => "result: " + r, identity)
      g onComplete {
        case Success(_) => done(false)
        case Failure(t) => done(t.getMessage() == "expected")
      }
  }

  def testFlatMapSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f flatMap { _ => Future { 10 } }
      g onComplete {
        case Success(x) => done(x == 10)
        case Failure(_) => done(false)
      }
  }

  def testFlatMapFailure(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception("expected") }
      val g = f flatMap { _ => Future { 10 } }
      g onComplete {
        case Success(_) => done(false)
        case Failure(t) => done(t.getMessage() == "expected")
      }
  }

  def testFlatMapDelayed(): Unit = once {
    done =>
      val f = Future { 5 }
      val p = Promise[Int]()
      val g = f flatMap { _ => p.future }
      g onComplete {
        case Success(x) => done(x == 10)
        case Failure(_) => done(false)
      }
      p.success(10)
  }

  def testFilterSuccess(): Unit = once {
    done =>
      val f = Future { 4 }
      val g = f filter { _ % 2 == 0 }
      g onComplete {
        case Success(x: Int) => done(x == 4)
        case Failure(_) => done(false)
      }
  }

  def testFilterFailure(): Unit = once {
    done =>
      val f = Future { 4 }
      val g = f filter { _ % 2 == 1 }
      g onComplete {
        case Success(x: Int) => done(false)
        case Failure(e: NoSuchElementException) => done(true)
        case Failure(_) => done(false)
      }
  }

  def testCollectSuccess(): Unit = once {
    done =>
      val f = Future { -5 }
      val g = f collect { case x if x < 0 => -x }
      g onComplete {
        case Success(x: Int) => done(x == 5)
        case Failure(_) => done(false)
      }
  }

  def testCollectFailure(): Unit = once {
    done =>
      val f = Future { -5 }
      val g = f collect { case x if x > 0 => x * 2 }
      g onComplete {
        case Success(_) => done(false)
        case Failure(e: NoSuchElementException) => done(true)
        case Failure(_) => done(false)
      }
  }

  def testForeachSuccess(): Unit = once {
    done =>
      val p = Promise[Int]()
      val f = Future[Int] { 5 }
      f foreach { x => p.success(x * 2) }
      val g = p.future

      g onComplete {
        case Success(res: Int) => done(res == 10)
        case Failure(_) => done(false)
      }
  }

  def testForeachFailure(): Unit = once {
    done =>
      val p = Promise[Int]()
      val f = Future[Int] { throw new Exception }
      f foreach { x => p.success(x * 2) }
      f onComplete { case Failure(_) => p.failure(new Exception); case _ => }
      val g = p.future

      g onComplete {
        case Success(_) => done(false)
        case Failure(_) => done(true)
      }
  }

  def testRecoverSuccess(): Unit = once {
    done =>
      val cause = new RuntimeException
      val f = Future {
        throw cause
      } recover {
        case re: RuntimeException =>
          "recovered" }
      f onComplete {
        case Success(x) => done(x == "recovered")
        case Failure(_) => done(false)
      }
  }

  def testRecoverFailure(): Unit = once {
    done =>
      val cause = new RuntimeException
      val f = Future {
        throw cause
      } recover {
        case te: TimeoutException => "timeout"
      }
      f onComplete {
        case Success(_) => done(false)
        case Failure(any) => done(any == cause)
      }
  }

  def testRecoverWithSuccess(): Unit = once {
    done =>
      val cause = new RuntimeException
      val f = Future {
        throw cause
      } recoverWith {
        case re: RuntimeException =>
          Future { "recovered" }
      }
      f onComplete {
        case Success(x) => done(x == "recovered")
        case Failure(_) => done(false)
      }
  }

  def testRecoverWithFailure(): Unit = once {
    done =>
      val cause = new RuntimeException
      val f = Future {
        throw cause
      } recoverWith {
        case te: TimeoutException =>
          Future { "timeout" }
      }
      f onComplete {
        case Success(_) => done(false)
        case Failure(any) => done(any == cause)
      }
  }

  def testZipSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = Future { 6 }
      val h = f zip g
      h onComplete {
        case Success((l: Int, r: Int)) => done(l + r == 11)
        case Success(_) =>
        case Failure(_) => done(false)
      }
  }

  def testZipFailureLeft(): Unit = once {
    done =>
      val cause = new Exception("expected")
      val f = Future { throw cause }
      val g = Future { 6 }
      val h = f zip g
      h onComplete {
        case Success(_) => done(false)
        case Failure(e: Exception) => done(e.getMessage == "expected")
        case Failure(_) =>
      }
  }

  def testZipFailureRight(): Unit = once {
    done =>
      val cause = new Exception("expected")
      val f = Future { 5 }
      val g = Future { throw cause }
      val h = f zip g
      h onComplete {
        case Success(_) => done(false)
        case Failure(e: Exception) => done(e.getMessage == "expected")
        case Failure(_) =>
      }
  }

  def testFallbackTo(): Unit = once {
    done =>
      val f = Future { sys.error("failed") }
      val g = Future { 5 }
      val h = f fallbackTo g
      h onComplete {
        case Success(x: Int) => done(x == 5)
        case Failure(_) => done(false)
      }
  }

  def testFallbackToFailure(): Unit = once {
    done =>
      val cause = new Exception
      val f = Future { throw cause }
      val g = Future { sys.error("failed") }
      val h = f fallbackTo g

      h onComplete {
        case Success(_) => done(false)
        case Failure(e) => done(e eq cause)
      }
  }

  def testFallbackToThis(): Unit = {
    def check(f: Future[Int]) = assert((f fallbackTo f) eq f)

    check(Future { 1 })
    check(Future.successful(1))
    check(Future.failed[Int](new Exception))
  }

  private[this] final def testMulti(f: Future[String] => Future[String]): Unit = {
    val p = Promise[String]()
    val f1, f2, f3 = f(p.future)

    val p2 = Promise[String]()
    val f4, f5, f6 = f(p.future)

    p.success("foo")
    p2.success("bar")

    List(f1,f2,f3).foreach(f => Await.ready(f, 2.seconds))
    assert(f1.value == f2.value && f2.value == f2.value)
    List(f4,f5,f6).foreach(f => Await.ready(f, 2.seconds))
    assert(f4.value == f5.value && f5.value == f6.value)
  }

  def testMultiFlatMap(): Unit = testMulti((to) => Future.unit.flatMap(_ => to))
  def testMultiRecoverWith(): Unit = testMulti((to) => Future.failed[String](new NullPointerException).recoverWith { case _ => to })
  def testMultiTransformWith(): Unit = testMulti((to) => Future.unit.transformWith(_ => to))

  test("testMapSuccess")(testMapSuccess())
  test("testMapFailure")(testMapFailure())
  test("testFlatMapSuccess")(testFlatMapSuccess())
  test("testFlatMapFailure")(testFlatMapFailure())
  test("testFlatMapDelayed")(testFlatMapDelayed())
  test("testFilterSuccess")(testFilterSuccess())
  test("testFilterFailure")(testFilterFailure())
  test("testCollectSuccess")(testCollectSuccess())
  test("testCollectFailure")(testCollectFailure())
  test("testForeachSuccess")(testForeachSuccess())
  test("testForeachFailure")(testForeachFailure())
  test("testRecoverSuccess")(testRecoverSuccess())
  test("testRecoverFailure")(testRecoverFailure())
  test("testRecoverWithSuccess")(testRecoverWithSuccess())
  test("testRecoverWithFailure")(testRecoverWithFailure())
  test("testZipSuccess")(testZipSuccess())
  test("testZipFailureLeft")(testZipFailureLeft())
  test("testZipFailureRight")(testZipFailureRight())
  test("testFallbackTo")(testFallbackTo())
  test("testFallbackToFailure")(testFallbackToFailure())
  test("testTransformSuccess")(testTransformSuccess())
  test("testTransformSuccessPF")(testTransformSuccessPF())
  test("testTransformFailure")(testTransformFailure())
  test("testTransformFailurePF")(testTransformFailurePF())
  test("testTransformResultToResult")(testTransformResultToResult())
  test("testTransformResultToFailure")(testTransformResultToFailure())
  test("testTransformFailureToResult")(testTransformFailureToResult())
  test("testTransformFailureToFailure")(testTransformFailureToFailure())
  test("testTransformWithResultToResult")(testTransformWithResultToResult())
  test("testTransformWithResultToFailure")(testTransformWithResultToFailure())
  test("testTransformWithFailureToResult")(testTransformWithFailureToResult())
  test("testTransformWithFailureToFailure")(testTransformWithFailureToFailure())
  test("testMultiFlatMap")(testMultiFlatMap())
  test("testMultiRecoverWith")(testMultiRecoverWith())
  test("testMultiTransformWith")(testMultiTransformWith())
}


class FutureProjections extends TestBase {
  import ExecutionContext.Implicits._

  def testFailedFailureOnComplete(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future { throw cause }
    f.failed onComplete {
      case Success(t) => done(t == cause)
      case Failure(t) => done(false)
    }
  }

  def testFailedFailureOnSuccess(): Unit = once {
    done =>
      val cause = new RuntimeException
      val f = Future { throw cause }
      f.failed foreach { t => done(t == cause) }
  }

  def testFailedSuccessOnComplete(): Unit = once {
    done =>
    val f = Future { 0 }
    f.failed onComplete {
      case Failure(_: NoSuchElementException) => done(true)
      case _ => done(false)
    }
  }

  def testFailedSuccessOnFailure(): Unit = once {
    done =>
      val f = Future { 0 }
      f.failed onComplete {
        case Success(_) => done(false)
        case Failure(_: NoSuchElementException) => done(true)
        case Failure(_) => done(false)
      }
  }

  def testFailedFailureAwait(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future { throw cause }
    done(await(f.failed) == cause)
  }

  def testFailedSuccessAwait(): Unit = once {
    done =>
    val f = Future { 0 }
    try {
      await(f.failed)
      done(false)
    } catch {
      case nsee: NoSuchElementException => done(true)
      case _: Throwable => done(false)
    }
  }

  def testAwaitPositiveDuration(): Unit = once { done =>
    val p = Promise[Int]()
    val f = p.future
    Future {
      assertThrows[IllegalArgumentException] { Await.ready(f, Duration.Undefined) }
      p.success(0)
      await(f)
      Await.ready(f, Duration.Zero)
      Await.ready(f, Duration(500, "ms"))
      Await.ready(f, Duration.Inf)
      done(true)
    } onComplete { case Failure(x) => done(throw x); case _ => }
  }

  def testAwaitNegativeDuration(): Unit = once { done =>
    val f = Promise().future
    Future {
      assertThrows[TimeoutException] { Await.ready(f, Duration.Zero) }
      assertThrows[TimeoutException] { Await.ready(f, Duration.MinusInf) }
      assertThrows[TimeoutException] { Await.ready(f, Duration(-500, "ms")) }
      done(true)
    } onComplete { case Failure(x) => done(throw x); case _ => }
  }

  test("testFailedFailureOnComplete")(testFailedFailureOnComplete())
  test("testFailedFailureOnSuccess")(testFailedFailureOnSuccess())
  test("testFailedSuccessOnComplete")(testFailedSuccessOnComplete())
  test("testFailedSuccessOnFailure")(testFailedSuccessOnFailure())
  test("testFailedFailureAwait")(testFailedFailureAwait())
  test("testFailedSuccessAwait")(testFailedSuccessAwait())
  test("testAwaitPositiveDuration")(testAwaitPositiveDuration())
  test("testAwaitNegativeDuration")(testAwaitNegativeDuration())
}


class Blocking extends TestBase {
  import ExecutionContext.Implicits._

  def testAwaitSuccess(): Unit = once {
    done =>
    val f = Future { 0 }
    done(await(f) == 0)
  }

  def testAwaitFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future { throw cause }
    try {
      await(f)
      done(false)
    } catch {
      case  t: Throwable => done(t == cause)
    }
  }

  def testFQCNForAwaitAPI(): Unit = once {
    done =>
    done(classOf[CanAwait].getName == "scala.concurrent.CanAwait" &&
         Await.getClass.getName == "scala.concurrent.Await$")
  }

  test("testAwaitSuccess")(testAwaitSuccess())
  test("testAwaitFailure")(testAwaitFailure())
  test("testFQCNForAwaitAPI")(testFQCNForAwaitAPI())
}

class BlockContexts extends TestBase {
  import ExecutionContext.Implicits._
  import scala.concurrent.BlockContext

  private def getBlockContext(body: => BlockContext): BlockContext = await(Future(body))

  // test outside of an ExecutionContext
  def testDefaultOutsideFuture(): Unit = {
    val bc = BlockContext.current
    assert(bc.getClass.getName.contains("DefaultBlockContext"))
  }

  // test BlockContext in our default ExecutionContext
  def testDefaultFJP(): Unit = {
    val prevCurrent = BlockContext.current
    val bc = getBlockContext(BlockContext.current)
    assert(bc ne prevCurrent) // Should have been replaced by the EC.
  }

  // test BlockContext inside BlockContext.withBlockContext
  def testPushCustom(): Unit = {
    val orig = BlockContext.current
    val customBC = new BlockContext() {
      override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = orig.blockOn(thunk)
    }

    val bc = getBlockContext({
      BlockContext.withBlockContext(customBC) {
        BlockContext.current
      }
    })

    assert(bc eq customBC)
  }

  // test BlockContext after a BlockContext.push
  def testPopCustom(): Unit = {
    val orig = BlockContext.current
    val customBC = new BlockContext() {
      override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = orig.blockOn(thunk)
    }

    val bc = getBlockContext({
      BlockContext.withBlockContext(customBC) {}
      BlockContext.current
    })

    assert(bc ne customBC)
  }

  test("testDefaultOutsideFuture")(testDefaultOutsideFuture())
  test("testDefaultFJP")(testDefaultFJP())
  test("testPushCustom")(testPushCustom())
  test("testPopCustom")(testPopCustom())
}

class Promises extends TestBase {
  import ExecutionContext.Implicits._

  def testSuccess(): Unit = once {
    done =>
      val p = Promise[Int]()
      val f = p.future

      f onComplete {
        case Success(x) => done(x == 5)
        case Failure(_) => done(false)
      }

      p.success(5)
  }

  def testFailure(): Unit = once {
    done =>
      val e = new Exception("expected")
      val p = Promise[Int]()
      val f = p.future

      f onComplete {
        case Success(_) => done(false)
        case Failure(any) => done(any eq e)
      }

      p.failure(e)
  }

  test("testSuccess")(testSuccess())
  test("testFailure")(testFailure())
}


class Exceptions extends TestBase {
  import java.util.concurrent.{Executors, RejectedExecutionException}
  def interruptHandling(): Unit = {
    implicit val e = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
    val p = Promise[String]()
    val f = p.future.map(_ => Thread.sleep(2000))
    p.success("foo")
    Thread.sleep(20)
    e.shutdownNow()

    val Failure(ee: ExecutionException) = Await.ready(f, 2.seconds).value.get
    assert(ee.getCause.isInstanceOf[InterruptedException])
  }

  def rejectedExecutionException(): Unit = {
    implicit val e = ExecutionContext.fromExecutor((r: Runnable) => throw new RejectedExecutionException("foo"))
    val p = Promise[String]()
    p.success("foo")
    val f = p.future.map(identity)
    val Failure(t: RejectedExecutionException) = Await.ready(f, 2.seconds).value.get
  }

  test("interruptHandling")(interruptHandling())
  test("rejectedExecutionException")(rejectedExecutionException())
}

class GlobalExecutionContext extends TestBase {
  import ExecutionContext.Implicits._

  @nowarn("cat=deprecation")  // Thread.getID is deprecated since JDK 19
  def testNameOfGlobalECThreads(): Unit = once {
    done => Future({
        val expectedName = "scala-execution-context-global-"+ Thread.currentThread.getId
        done(expectedName == Thread.currentThread.getName)
      })(ExecutionContext.global)
  }

  test("testNameOfGlobalECThreads")(testNameOfGlobalECThreads())
}

class CustomExecutionContext extends TestBase {

  def defaultEC = ExecutionContext.global

  val inEC = new java.lang.ThreadLocal[Int]() {
    override def initialValue = 0
  }

  def enterEC() = inEC.set(inEC.get + 1)
  def leaveEC() = inEC.set(inEC.get - 1)
  def assertEC() = assert(inEC.get > 0)
  def assertNoEC() = assert(inEC.get == 0)

  class CountingExecutionContext extends ExecutionContext {
    val _count = new java.util.concurrent.atomic.AtomicInteger(0)
    def count = _count.get

    def delegate = ExecutionContext.global

    override def execute(runnable: Runnable) = {
      _count.incrementAndGet()
      val wrapper = new Runnable() {
        override def run() = {
          enterEC()
          try {
            runnable.run()
          } finally {
            leaveEC()
          }
        }
      }
      delegate.execute(wrapper)
    }

    override def reportFailure(t: Throwable): Unit = {
      System.err.println("Failure: " + t.getClass.getSimpleName + ": " + t.getMessage)
      delegate.reportFailure(t)
    }
  }

  def countExecs(block: (ExecutionContext) => Unit): Int = {
    val context = new CountingExecutionContext()
    block(context)
    context.count
  }

  def testOnSuccessCustomEC(): Unit = {
    val count = countExecs { implicit ec =>
      blocking {
        once { done =>
          val f = Future(assertNoEC())(defaultEC)
          f foreach { _ =>
            assertEC()
            done(true)
          }
          assertNoEC()
        }
      }
    }

    // should be onSuccess, but not future body
    assert(count == 1)
  }

  def testKeptPromiseCustomEC(): Unit = {
    val count = countExecs { implicit ec =>
      blocking {
        once { done =>
          val f = Promise.successful(10).future
          f foreach { _ =>
            assertEC()
            done(true)
          }
        }
      }
    }

    // should be onSuccess called once in proper EC
    assert(count == 1)
  }

  def testCallbackChainCustomEC(): Unit = {
    val count = countExecs { implicit ec =>
      blocking {
        once { done =>
          assertNoEC()
          val addOne = { x: Int => assertEC(); x + 1 }
          val f = Promise.successful(10).future
          f.map(addOne).filter { x =>
             assertEC()
             x == 11
           } flatMap { x =>
             Promise.successful(x + 1).future.map(addOne).map(addOne)
           } onComplete {
            case Failure(t) =>
              done(throw new AssertionError("error in test: " + t.getMessage, t))
            case Success(x) =>
              assertEC()
              done(x == 14)
          }
          assertNoEC()
        }
      }
    }

    // the count is not defined (other than >=1)
    // due to the batching optimizations.
    assert(count >= 1)
  }

  test("testOnSuccessCustomEC")(testOnSuccessCustomEC())
  test("testKeptPromiseCustomEC")(testKeptPromiseCustomEC())
  test("testCallbackChainCustomEC")(testCallbackChainCustomEC())
}

class ExecutionContextPrepare extends TestBase {
  val theLocal = new ThreadLocal[String] {
    override protected def initialValue(): String = ""
  }

  class PreparingExecutionContext extends ExecutionContext {
    def delegate = ExecutionContext.global

    override def execute(runnable: Runnable): Unit =
      delegate.execute(runnable)

    override def prepare(): ExecutionContext = {
      // save object stored in ThreadLocal storage
      val localData = theLocal.get
      new PreparingExecutionContext {
        override def execute(runnable: Runnable): Unit = {
          val wrapper = new Runnable {
            override def run(): Unit = {
              // now we're on the new thread
              // put localData into theLocal
              theLocal.set(localData)
              runnable.run()
            }
          }
          delegate.execute(wrapper)
        }
      }
    }

    override def reportFailure(t: Throwable): Unit =
      delegate.reportFailure(t)
  }

  implicit val ec: ExecutionContext = new PreparingExecutionContext

  def testOnComplete(): Unit = once {
    done =>
    theLocal.set("secret")
    val fut = Future { 42 }
    fut onComplete { case _ => done(theLocal.get == "secret") }
  }

  def testMap(): Unit = once {
    done =>
    theLocal.set("secret2")
    val fut = Future { 42 }
    fut map { x => done(theLocal.get == "secret2") }
  }

  test("testOnComplete")(testOnComplete())
  test("testMap")(testMap())
}

object Test
extends App {
  new FutureCallbacks
  new FutureCombinators
  new FutureProjections
  new Promises
  new Blocking
  new BlockContexts
  new Exceptions
  new GlobalExecutionContext
  new CustomExecutionContext
  new ExecutionContextPrepare

  System.exit(0)
}
