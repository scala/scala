import scala.concurrent.{
  Future,
  Promise,
  TimeoutException,
  SyncVar,
  ExecutionException,
  ExecutionContext,
  CanAwait,
  Await
}
import scala.concurrent.blocking
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration.Duration
import scala.reflect.{ classTag, ClassTag }
import scala.tools.partest.TestUtil.intercept

trait TestBase {
  trait Done { def apply(proof: => Boolean): Unit }
  def once(body: Done => Unit) {
    import java.util.concurrent.{ LinkedBlockingQueue, TimeUnit }
    val q = new LinkedBlockingQueue[Try[Boolean]]
    body(new Done {
      def apply(proof: => Boolean): Unit = q offer Try(proof)
    })
    assert(q.poll(2000, TimeUnit.MILLISECONDS).get)
    // Check that we don't get more than one completion
    assert(q.poll(50, TimeUnit.MILLISECONDS) eq null)
  }
}


trait FutureCallbacks extends TestBase {
  import ExecutionContext.Implicits._

  def testOnSuccess(): Unit = once {
    done =>
    var x = 0
    val f = Future { x = 1 }
    f onSuccess { case _ => done(x == 1) }
  }

  def testOnSuccessWhenCompleted(): Unit = once {
    done =>
    var x = 0
    val f = Future { x = 1 }
    f onSuccess {
      case _ if x == 1 =>
      x = 2
      f onSuccess {  case _ => done(x == 2) }
    }
  }

  def testOnSuccessWhenFailed(): Unit = once {
    done =>
    val f = Future[Unit] { throw new Exception }
    f onSuccess { case _ => done(false) }
    f onFailure { case _ => done(true) }
  }

  def testOnFailure(): Unit = once {
    done =>
    val f = Future[Unit] { throw new Exception }
    f onSuccess { case _ => done(false) }
    f onFailure { case _ => done(true) }
  }

  def testOnFailureWhenSpecialThrowable(num: Int, cause: Throwable): Unit = once {
    done =>
    val f = Future[Unit] { throw cause }
    f onSuccess { case _ => done(false) }
    f onFailure {
      case e: ExecutionException if e.getCause == cause => done(true)
      case _ => done(false)
    }
  }

  def testOnFailureWhenTimeoutException(): Unit = once {
    done =>
    val f = Future[Unit] { throw new TimeoutException() }
    f onSuccess { case _ => done(false) }
    f onFailure {
      case e: TimeoutException => done(true)
      case _ => done(false)
    }
  }

  def testThatNestedCallbacksDoNotYieldStackOverflow(): Unit = {
    val promise = Promise[Int]
    (0 to 10000).map(Future(_)).foldLeft(promise.future)((f1, f2) => f2.flatMap(i => f1))
    promise.success(-1)
  }

  def stressTestNumberofCallbacks(): Unit = once {
    done =>
      val promise = Promise[Unit]
      val otherPromise = Promise[Unit]
      def attachMeaninglessCallbacksTo(f: Future[Any]): Unit = (1 to 1000).foreach(_ => f.onComplete(_ => ()))
      attachMeaninglessCallbacksTo(promise.future)
      val future = promise.future.flatMap { _ =>
        attachMeaninglessCallbacksTo(otherPromise.future)
        otherPromise.future
      }
      val numbers = new java.util.concurrent.ConcurrentHashMap[Int, Unit]()
      (0 to 10000) foreach { x => numbers.put(x, ()) }
      Future.sequence((0 to 10000) map { x => future.andThen({ case _ => numbers.remove(x) }) }) onComplete {
        _ => done(numbers.isEmpty)
      }
      promise.success(())
      otherPromise.success(())
  }

  testOnSuccess()
  testOnSuccessWhenCompleted()
  testOnSuccessWhenFailed()
  testOnFailure()
  testOnFailureWhenSpecialThrowable(5, new Error)
  // testOnFailureWhenSpecialThrowable(6, new scala.util.control.ControlThrowable { })
  //TODO: this test is currently problematic, because NonFatal does not match InterruptedException
  //testOnFailureWhenSpecialThrowable(7, new InterruptedException)
  testThatNestedCallbacksDoNotYieldStackOverflow()
  testOnFailureWhenTimeoutException()
  stressTestNumberofCallbacks()
}


trait FutureCombinators extends TestBase {
  import ExecutionContext.Implicits._

  def testMapSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f map { x => "result: " + x }
      g onSuccess { case s => done(s == "result: 5") }
      g onFailure { case _ => done(false) }
  }

  def testMapFailure(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception("exception message") }
      val g = f map { x => "result: " + x }
      g onSuccess { case _ => done(false) }
      g onFailure { case t => done(t.getMessage() == "exception message") }
  }

  def testMapSuccessPF(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f map { case r => "result: " + r }
      g onSuccess { case s => done(s == "result: 5") }
      g onFailure { case _ => done(false) }
  }

  def testTransformSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f.transform(r => "result: " + r, identity)
      g onSuccess { case s => done(s == "result: 5") }
      g onFailure { case _ => done(false) }
  }

  def testTransformSuccessPF(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f.transform( { case r => "result: " + r }, identity)
      g onSuccess { case s => done(s == "result: 5") }
      g onFailure { case _ => done(false) }
  }

def testTransformFailure(): Unit = once {
    done =>
      val transformed = new Exception("transformed")
      val f = Future { throw new Exception("expected") }
      val g = f.transform(identity, _ => transformed)
      g onSuccess { case _ => done(false) }
      g onFailure { case e => done(e eq transformed) }
  }

  def testTransformFailurePF(): Unit = once {
    done =>
      val e = new Exception("expected")
      val transformed = new Exception("transformed")
      val f = Future[Unit] { throw e }
      val g = f.transform(identity, { case `e` => transformed })
      g onSuccess { case _ => done(false) }
      g onFailure { case e => done(e eq transformed) }
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
      g onSuccess { case _ => done(false) }
      g onFailure { case t => done(t.getMessage() == "expected") }
  }

  def testFlatMapSuccess(): Unit = once {
    done =>
      val f = Future { 5 }
      val g = f flatMap { _ => Future { 10 } }
      g onSuccess { case x => done(x == 10) }
      g onFailure { case _ => done(false) }
  }

  def testFlatMapFailure(): Unit = once {
    done =>
      val f = Future[Unit] { throw new Exception("expected") }
      val g = f flatMap { _ => Future { 10 } }
      g onSuccess { case _ => done(false) }
      g onFailure { case t => done(t.getMessage() == "expected") }
  }

  def testFlatMapDelayed(): Unit = once {
    done =>
      val f = Future { 5 }
      val p = Promise[Int]
      val g = f flatMap { _ => p.future }
      g onSuccess { case x => done(x == 10) }
      g onFailure { case _ => done(false) }
      p.success(10)
  }

  def testFilterSuccess(): Unit = once {
    done =>
      val f = Future { 4 }
      val g = f filter { _ % 2 == 0 }
      g onSuccess { case x: Int => done(x == 4) }
      g onFailure { case _ => done(false) }
  }

  def testFilterFailure(): Unit = once {
    done =>
      val f = Future { 4 }
      val g = f filter { _ % 2 == 1 }
      g onSuccess { case x: Int => done(false) }
      g onFailure {
        case e: NoSuchElementException => done(true)
        case _ => done(false)
      }
  }

  def testCollectSuccess(): Unit = once {
    done =>
      val f = Future { -5 }
      val g = f collect { case x if x < 0 => -x }
      g onSuccess { case x: Int => done(x == 5) }
      g onFailure { case _ => done(false) }
  }

  def testCollectFailure(): Unit = once {
    done =>
      val f = Future { -5 }
      val g = f collect { case x if x > 0 => x * 2 }
      g onSuccess { case _ => done(false) }
      g onFailure {
        case e: NoSuchElementException => done(true)
        case _ => done(false)
      }
  }

  /* TODO: Test for NonFatal in collect (more of a regression test at this point).
   */

  def testForeachSuccess(): Unit = once {
    done =>
      val p = Promise[Int]()
      val f = Future[Int] { 5 }
      f foreach { x => p.success(x * 2) }
      val g = p.future

      g.onSuccess { case res: Int => done(res == 10) }
      g.onFailure { case _ => done(false) }
  }

  def testForeachFailure(): Unit = once {
    done =>
      val p = Promise[Int]()
      val f = Future[Int] { throw new Exception }
      f foreach { x => p.success(x * 2) }
      f onFailure { case _ => p.failure(new Exception) }
      val g = p.future

      g.onSuccess { case _ => done(false) }
      g.onFailure { case _ => done(true) }
  }

  def testRecoverSuccess(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future {
      throw cause
    } recover {
      case re: RuntimeException =>
        "recovered" }
    f onSuccess { case x => done(x == "recovered") }
    f onFailure { case any => done(false) }
  }

  def testRecoverFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future {
      throw cause
    } recover {
      case te: TimeoutException => "timeout"
    }
    f onSuccess { case _ => done(false) }
    f onFailure { case any => done(any == cause) }
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
    f onSuccess { case x => done(x == "recovered") }
    f onFailure { case any => done(false) }
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
    f onSuccess { case x => done(false) }
    f onFailure { case any => done(any == cause) }
  }

  def testZipSuccess(): Unit = once {
    done =>
    val f = Future { 5 }
    val g = Future { 6 }
    val h = f zip g
    h onSuccess { case (l: Int, r: Int) => done(l+r == 11) }
    h onFailure { case _ => done(false) }
  }

  def testZipFailureLeft(): Unit = once {
    done =>
    val cause = new Exception("expected")
    val f = Future { throw cause }
    val g = Future { 6 }
    val h = f zip g
    h onSuccess { case _ => done(false) }
    h onFailure { case e: Exception => done(e.getMessage == "expected") }
  }

  def testZipFailureRight(): Unit = once {
    done =>
    val cause = new Exception("expected")
    val f = Future { 5 }
    val g = Future { throw cause }
    val h = f zip g
    h onSuccess { case _ => done(false) }
    h onFailure { case e: Exception => done(e.getMessage == "expected") }
  }

  def testFallbackTo(): Unit = once {
    done =>
    val f = Future { sys.error("failed") }
    val g = Future { 5 }
    val h = f fallbackTo g
    h onSuccess { case x: Int => done(x == 5) }
    h onFailure { case _ => done(false) }
  }

  def testFallbackToFailure(): Unit = once {
    done =>
    val cause = new Exception
    val f = Future { throw cause }
    val g = Future { sys.error("failed") }
    val h = f fallbackTo g

    h onSuccess { case _ => done(false) }
    h onFailure { case e => done(e eq cause) }
  }

  def testFallbackToThis(): Unit = {
    def check(f: Future[Int]) = assert((f fallbackTo f) eq f)

    check(Future { 1 })
    check(Future.successful(1))
    check(Future.failed[Int](new Exception))
  }

  testMapSuccess()
  testMapFailure()
  testFlatMapSuccess()
  testFlatMapFailure()
  testFlatMapDelayed()
  testFilterSuccess()
  testFilterFailure()
  testCollectSuccess()
  testCollectFailure()
  testForeachSuccess()
  testForeachFailure()
  testRecoverSuccess()
  testRecoverFailure()
  testRecoverWithSuccess()
  testRecoverWithFailure()
  testZipSuccess()
  testZipFailureLeft()
  testZipFailureRight()
  testFallbackTo()
  testFallbackToFailure()
  testTransformSuccess()
  testTransformSuccessPF()
  testTransformFailure()
  testTransformFailurePF()
  testTransformResultToResult()
  testTransformResultToFailure()
  testTransformFailureToResult()
  testTransformFailureToFailure()
  testTransformWithResultToResult()
  testTransformWithResultToFailure()
  testTransformWithFailureToResult()
  testTransformWithFailureToFailure()
}


trait FutureProjections extends TestBase {
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
    f.failed onSuccess { case t => done(t == cause) }
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
    f.failed onFailure {
      case e: NoSuchElementException => done(true)
      case _ => done(false)
    }
    f.failed onSuccess { case _ => done(false) }
  }

  def testFailedFailureAwait(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future { throw cause }
    done(Await.result(f.failed, Duration(500, "ms")) == cause)
  }

  def testFailedSuccessAwait(): Unit = once {
    done =>
    val f = Future { 0 }
    try {
      Await.result(f.failed, Duration(500, "ms"))
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
      intercept[IllegalArgumentException] { Await.ready(f, Duration.Undefined) }
      p.success(0)
      Await.ready(f, Duration.Zero)
      Await.ready(f, Duration(500, "ms"))
      Await.ready(f, Duration.Inf)
      done(true)
    } onFailure { case x => done(throw x) }
  }

  def testAwaitNegativeDuration(): Unit = once { done =>
    val f = Promise().future
    Future {
      intercept[TimeoutException] { Await.ready(f, Duration.Zero) }
      intercept[TimeoutException] { Await.ready(f, Duration.MinusInf) }
      intercept[TimeoutException] { Await.ready(f, Duration(-500, "ms")) }
      done(true)
    } onFailure { case x => done(throw x) }
  }

  testFailedFailureOnComplete()
  testFailedFailureOnSuccess()
  testFailedSuccessOnComplete()
  testFailedSuccessOnFailure()
  testFailedFailureAwait()
  testFailedSuccessAwait()
  testAwaitPositiveDuration()
  testAwaitNegativeDuration()
}


trait Blocking extends TestBase {
  import ExecutionContext.Implicits._

  def testAwaitSuccess(): Unit = once {
    done =>
    val f = Future { 0 }
    done(Await.result(f, Duration(500, "ms")) == 0)
  }

  def testAwaitFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = Future { throw cause }
    try {
      Await.result(f, Duration(500, "ms"))
      done(false)
    } catch {
      case  t: Throwable => done(t == cause)
    }
  }

  def testFQCNForAwaitAPI(): Unit = once {
    done =>
    done(classOf[CanAwait].getName == "scala.concurrent.CanAwait" &&
         Await.getClass.getName == "scala.concurrent.Await")
  }

  testAwaitSuccess()
  testAwaitFailure()
  testFQCNForAwaitAPI()
}

trait BlockContexts extends TestBase {
  import ExecutionContext.Implicits._
  import scala.concurrent.{ Await, Awaitable, BlockContext }

  private def getBlockContext(body: => BlockContext): BlockContext = {
    Await.result(Future { body }, Duration(500, "ms"))
  }

  // test outside of an ExecutionContext
  def testDefaultOutsideFuture(): Unit = {
    val bc = BlockContext.current
    assert(bc.getClass.getName.contains("DefaultBlockContext"))
  }

  // test BlockContext in our default ExecutionContext
  def testDefaultFJP(): Unit = {
    val bc = getBlockContext(BlockContext.current)
    assert(bc.isInstanceOf[java.util.concurrent.ForkJoinWorkerThread])
  }

  // test BlockContext inside BlockContext.withBlockContext
  def testPushCustom(): Unit = {
    val orig = BlockContext.current
    val customBC = new BlockContext() {
      override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T = orig.blockOn(thunk)
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
      override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T = orig.blockOn(thunk)
    }

    val bc = getBlockContext({
      BlockContext.withBlockContext(customBC) {}
      BlockContext.current
    })

    assert(bc ne customBC)
  }

  testDefaultOutsideFuture()
  testDefaultFJP()
  testPushCustom()
  testPopCustom()
}

trait Promises extends TestBase {
  import ExecutionContext.Implicits._

  def testSuccess(): Unit = once {
    done =>
    val p = Promise[Int]()
    val f = p.future

    f onSuccess { case x => done(x == 5) }
    f onFailure { case any => done(false) }

    p.success(5)
  }

  def testFailure(): Unit = once {
    done =>
    val e = new Exception("expected")
    val p = Promise[Int]()
    val f = p.future

    f onSuccess { case x => done(false) }
    f onFailure { case any => done(any eq e) }

    p.failure(e)
  }

  testSuccess()
  testFailure()
}


trait Exceptions extends TestBase {
  import ExecutionContext.Implicits._

}

trait GlobalExecutionContext extends TestBase {
  def testNameOfGlobalECThreads(): Unit = once {
    done => Future({
        val expectedName = "scala-execution-context-global-"+ Thread.currentThread.getId
        done(expectedName == Thread.currentThread.getName)
      })(ExecutionContext.global)
  }

  testNameOfGlobalECThreads()
}

trait CustomExecutionContext extends TestBase {
  import scala.concurrent.{ ExecutionContext, Awaitable }

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
          f onSuccess {
            case _ =>
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
          f onSuccess {
            case _ =>
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

  testOnSuccessCustomEC()
  testKeptPromiseCustomEC()
  testCallbackChainCustomEC()
}

trait ExecutionContextPrepare extends TestBase {
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

  implicit val ec = new PreparingExecutionContext

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

  testOnComplete()
  testMap()
}

object Test
extends App
with FutureCallbacks
with FutureCombinators
with FutureProjections
with Promises
with BlockContexts
with Exceptions
with GlobalExecutionContext
with CustomExecutionContext
with ExecutionContextPrepare
{
  System.exit(0)
}

