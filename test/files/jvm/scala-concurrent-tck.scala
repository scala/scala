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
import scala.concurrent.{ future, promise, blocking }
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration.Duration
import scala.reflect.{ classTag, ClassTag }
import scala.tools.partest.TestUtil.intercept

trait TestBase {
  
  def once(body: (() => Unit) => Unit) {
    val sv = new SyncVar[Boolean]
    body(() => sv put true)
    sv.take(2000)
  }

  // def assert(cond: => Boolean) {
  //   try {
  //     Predef.assert(cond)
  //   } catch {
  //     case e => e.printStackTrace()
  //   }
  // }

}


trait FutureCallbacks extends TestBase {
  import ExecutionContext.Implicits._

  def testOnSuccess(): Unit = once {
    done =>
    var x = 0
    val f = future {
      x = 1
    }
    f onSuccess {
      case _ =>
        done()
        assert(x == 1)
    }
  }
  
  def testOnSuccessWhenCompleted(): Unit = once {
    done =>
    var x = 0
    val f = future {
      x = 1
    }
    f onSuccess { 
      case _ =>
      assert(x == 1)
      x = 2
      f onSuccess { 
        case _ =>
          assert(x == 2)
          done()
      }
    }
  }

  def testOnSuccessWhenFailed(): Unit = once {
    done =>
    val f = future[Unit] {
      done()
      throw new Exception
    }
    f onSuccess {
      case _ => assert(false)
    }
  }
  
  def testOnFailure(): Unit = once {
    done =>
    var x = 0
    val f = future[Unit] {
      x = 1
      throw new Exception
    }
    f onSuccess {
      case _ =>
        done()
        assert(false)
    }
    f onFailure {
      case _ =>
        done()
        assert(x == 1)
    }
  }

  def testOnFailureWhenSpecialThrowable(num: Int, cause: Throwable): Unit = once {
    done =>
    val f = future[Unit] {
      throw cause
    }
    f onSuccess {
      case _ =>
        done()
        assert(false)
    }
    f onFailure {
      case e: ExecutionException if (e.getCause == cause) =>
        done()
      case _ =>
        done()
        assert(false)
    }
  }
  
  def testOnFailureWhenTimeoutException(): Unit = once {
    done =>
    val f = future[Unit] {
      throw new TimeoutException()
    }
    f onSuccess {
      case _ =>
        done()
        assert(false)
    }
    f onFailure {
      case e: TimeoutException =>
        done()
      case other =>
        done()
        assert(false)
    }
  }

  def testThatNestedCallbacksDoNotYieldStackOverflow(): Unit = {
    val promise = Promise[Int]
    (0 to 10000).map(Future(_)).foldLeft(promise.future)((f1, f2) => f2.flatMap(i => f1))
    promise.success(-1)
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
  
}


trait FutureCombinators extends TestBase {
  import ExecutionContext.Implicits._

  def testMapSuccess(): Unit = once {
    done =>
      val f = future { 5 }
      val g = f map { x => "result: " + x }
      g onSuccess {
        case s =>
          done()
          assert(s == "result: 5")
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testMapFailure(): Unit = once {
    done =>
      val f = future {
        throw new Exception("exception message")
      }
      val g = f map { x => "result: " + x }
      g onSuccess {
        case _ =>
          done()
          assert(false)
      }
      g onFailure {
        case t =>
          done()
          assert(t.getMessage() == "exception message")
      }
  }

  def testMapSuccessPF(): Unit = once {
    done =>
      val f = future { 5 }
      val g = f map { case r => "result: " + r }
      g onSuccess {
        case s =>
          done()
          assert(s == "result: 5")
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testTransformSuccess(): Unit = once {
    done =>
      val f = future { 5 }
      val g = f.transform(r => "result: " + r, identity)
      g onSuccess {
        case s =>
          done()
          assert(s == "result: 5")
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testTransformSuccessPF(): Unit = once {
    done =>
      val f = future { 5 }
      val g = f.transform( { case r => "result: " + r }, identity)
      g onSuccess {
        case s =>
          done()
          assert(s == "result: 5")
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testFoldFailure(): Unit = once {
    done =>
      val f = future {
        throw new Exception("exception message")
      }
      val g = f.transform(r => "result: " + r, identity)
      g onSuccess {
        case _ =>
          done()
          assert(false)
      }
      g onFailure {
        case t =>
          done()
          assert(t.getMessage() == "exception message")
      }
  }

  def testFlatMapSuccess(): Unit = once {
    done =>
      val f = future { 5 }
      val g = f flatMap { _ => future { 10 } }
      g onSuccess {
        case x =>
          done()
          assert(x == 10)
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testFlatMapFailure(): Unit = once {
    done =>
      val f = future {
        throw new Exception("exception message")
      }
      val g = f flatMap { _ => future { 10 } }
      g onSuccess {
        case _ =>
          done()
          assert(false)
      }
      g onFailure {
        case t =>
          done()
          assert(t.getMessage() == "exception message")
      }
  }

  def testFilterSuccess(): Unit = once {
    done =>
      val f = future { 4 }
      val g = f filter { _ % 2 == 0 }
      g onSuccess {
        case x: Int =>
          done()
          assert(x == 4)
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testFilterFailure(): Unit = once {
    done =>
      val f = future { 4 }
      val g = f filter { _ % 2 == 1 }
      g onSuccess {
        case x: Int =>
          done()
          assert(false)
      }
      g onFailure {
        case e: NoSuchElementException =>
          done()
          assert(true)
        case _ =>
          done()
          assert(false)
      }
  }

  def testCollectSuccess(): Unit = once {
    done =>
      val f = future { -5 }
      val g = f collect {
        case x if x < 0 => -x
      }
      g onSuccess {
        case x: Int =>
          done()
          assert(x == 5)
      }
      g onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testCollectFailure(): Unit = once {
    done =>
      val f = future { -5 }
      val g = f collect {
        case x if x > 0 => x * 2
      }
      g onSuccess {
        case _ =>
          done()
          assert(false)
      }
      g onFailure {
        case e: NoSuchElementException =>
          done()
          assert(true)
        case _ =>
          done()
          assert(false)
      }
  }

  /* TODO: Test for NonFatal in collect (more of a regression test at this point).
   */

  def testForeachSuccess(): Unit = once {
    done =>
      val p = promise[Int]()
      val f = future[Int] { 5 }
      f foreach { x => p.success(x * 2) }
      val g = p.future
      
      g.onSuccess {
        case res: Int =>
          done()
          assert(res == 10)
      }
      g.onFailure {
        case _ =>
          done()
          assert(false)
      }
  }

  def testForeachFailure(): Unit = once {
    done =>
      val p = promise[Int]()
      val f = future[Int] { throw new Exception }
      f foreach { x => p.success(x * 2) }
      f onFailure { case _ => p.failure(new Exception) }
      val g = p.future
      
      g.onSuccess {
        case _ =>
          done()
          assert(false)
      }
      g.onFailure {
        case _ =>
          done()
          assert(true)
      }
  }

  def testRecoverSuccess(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    } recover {
      case re: RuntimeException =>
        "recovered"
    }
    f onSuccess {
      case x =>
        done()
        assert(x == "recovered")
    }
    f onFailure { case any =>
      done()
      assert(false)
    }
    f
  }

  def testRecoverFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    } recover {
      case te: TimeoutException => "timeout"
    }
    f onSuccess {
      case x =>
        done()
        assert(false)
    }
    f onFailure { case any =>
      done()
      assert(any == cause)
    }
  }
  
  def testRecoverWithSuccess(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    } recoverWith {
      case re: RuntimeException =>
        future { "recovered" }
    }
    f onSuccess {
      case x =>
        done()
        assert(x == "recovered")
    }
    f onFailure { case any =>
      done()
      assert(false)
    }
  }

  def testRecoverWithFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    } recoverWith {
      case te: TimeoutException =>
        future { "timeout" }
    }
    f onSuccess {
      case x =>
        done()
        assert(false)
    }
    f onFailure { case any =>
      done()
      assert(any == cause)
    }
  }
 
  def testZipSuccess(): Unit = once {
    done =>
    val f = future { 5 }
    val g = future { 6 }
    val h = f zip g
    h onSuccess {
      case (l: Int, r: Int) =>
        done()
        assert(l+r == 11)
    }
    h onFailure {
      case _ =>
        done()
        assert(false)
    }
  }

  def testZipFailureLeft(): Unit = once {
    done =>
    val cause = new Exception("exception message")
    val f = future { throw cause }
    val g = future { 6 }
    val h = f zip g
    h onSuccess {
      case _ =>
        done()
        assert(false)
    }
    h onFailure {
      case e: Exception =>
        done()
        assert(e.getMessage == "exception message")
    }
  }

  def testZipFailureRight(): Unit = once {
    done =>
    val cause = new Exception("exception message")
    val f = future { 5 }
    val g = future { throw cause }
    val h = f zip g
    h onSuccess {
      case _ =>
        done()
        assert(false)
    }
    h onFailure {
      case e: Exception =>
        done()
        assert(e.getMessage == "exception message")
    }
  }

  def testFallbackTo(): Unit = once {
    done =>
    val f = future { sys.error("failed") }
    val g = future { 5 }
    val h = f fallbackTo g

    h onSuccess {
      case x: Int =>
        done()
        assert(x == 5)
    }
    h onFailure {
      case _ =>
        done()
        assert(false)
    }
  }

  def testFallbackToFailure(): Unit = once {
    done =>
    val cause = new Exception
    val f = future { sys.error("failed") }
    val g = future { throw cause }
    val h = f fallbackTo g

    h onSuccess {
      case _ =>
        done()
        assert(false)
    }
    h onFailure {
      case e: Exception =>
        done()
        assert(e == cause)
    }
  }

  testMapSuccess()
  testMapFailure()
  testFlatMapSuccess()
  testFlatMapFailure()
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
}


trait FutureProjections extends TestBase {
  import ExecutionContext.Implicits._

  def testFailedFailureOnComplete(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    f.failed onComplete {
      case Success(t) =>
        assert(t == cause)
        done()
      case Failure(t) =>
        assert(false)
    }
  }
  
  def testFailedFailureOnSuccess(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    f.failed onSuccess {
      case t =>
        assert(t == cause)
        done()
    }
  }
  
  def testFailedSuccessOnComplete(): Unit = once {
    done =>
    val f = future { 0 }
    f.failed onComplete {
      case Success(t) =>
        assert(false)
      case Failure(t) =>
        assert(t.isInstanceOf[NoSuchElementException])
        done()
    }
  }
  
  def testFailedSuccessOnFailure(): Unit = once {
    done =>
    val f = future { 0 }
    f.failed onFailure {
      case nsee: NoSuchElementException =>
      done()
    }
  }
  
  def testFailedFailureAwait(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    assert(Await.result(f.failed, Duration(500, "ms")) == cause)
    done()
  }
  
  def testFailedSuccessAwait(): Unit = once {
    done =>
    val f = future { 0 }
    try {
      Await.result(f.failed, Duration(500, "ms"))
      assert(false)
    } catch {
      case nsee: NoSuchElementException => done()
    }
  }

  def testAwaitPositiveDuration(): Unit = once { done =>
    val p = Promise[Int]()
    val f = p.future
    future {
      intercept[IllegalArgumentException] { Await.ready(f, Duration.Undefined) }
      p.success(0)
      Await.ready(f, Duration.Zero)
      Await.ready(f, Duration(500, "ms"))
      Await.ready(f, Duration.Inf)
      done()
    } onFailure { case x => throw x }
  }

  def testAwaitNegativeDuration(): Unit = once { done =>
    val f = Promise().future
    future {
      intercept[TimeoutException] { Await.ready(f, Duration.Zero) }
      intercept[TimeoutException] { Await.ready(f, Duration.MinusInf) }
      intercept[TimeoutException] { Await.ready(f, Duration(-500, "ms")) }
      done()
    } onFailure { case x => throw x }
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
    val f = future { 0 }
    Await.result(f, Duration(500, "ms"))
    done()
  }
  
  def testAwaitFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    try {
      Await.result(f, Duration(500, "ms"))
      assert(false)
    } catch {
      case t =>
        assert(t == cause)
        done()
    }
  }
  
  def testFQCNForAwaitAPI(): Unit = once {
    done =>
    
    assert(classOf[CanAwait].getName == "scala.concurrent.CanAwait")
    assert(Await.getClass.getName == "scala.concurrent.Await")
    
    done()
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
    assert(bc.isInstanceOf[scala.concurrent.forkjoin.ForkJoinWorkerThread])
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
    val p = promise[Int]()
    val f = p.future
    
    f onSuccess {
      case x =>
        done()
        assert(x == 5)
    }
    f onFailure {
      case any =>
        done()
        assert(false)
    }
    
    p.success(5)
  }

  testSuccess()

}


trait Exceptions extends TestBase {
  import ExecutionContext.Implicits._

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
          val f = future({ assertNoEC() })(defaultEC)
          f onSuccess {
            case _ =>
              assertEC()
            done()
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
            done()
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
              try {
                throw new AssertionError("error in test: " + t.getMessage, t)
              } finally {
                done()
              }
            case Success(x) =>
              assertEC()
              assert(x == 14)
              done()
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
    val fut = future { 42 }
    fut onComplete {
      case _ =>
        assert(theLocal.get == "secret")
        done()
    }
  }
  
  def testMap(): Unit = once {
    done =>
    theLocal.set("secret2")
    val fut = future { 42 }
    fut map { x =>
      assert(theLocal.get == "secret2")
      done()
    }
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
with CustomExecutionContext
with ExecutionContextPrepare
{
  System.exit(0)
}

