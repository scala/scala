import scala.concurrent.{
  Future,
  Promise,
  TimeoutException,
  SyncVar,
  ExecutionException
}
import scala.concurrent.future
import scala.concurrent.promise
import scala.concurrent.blocking
import scala.util.{ Try, Success, Failure }

import scala.concurrent.util.Duration


trait TestBase {
  
  def once(body: (() => Unit) => Unit) {
    val sv = new SyncVar[Boolean]
    body(() => sv put true)
    sv.take()
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
  
  testOnSuccess()
  testOnSuccessWhenCompleted()
  testOnSuccessWhenFailed()
  testOnFailure()
  testOnFailureWhenSpecialThrowable(5, new Error)
  testOnFailureWhenSpecialThrowable(6, new scala.util.control.ControlThrowable { })
  //TODO: this test is currently problematic, because NonFatal does not match InterruptedException
  //testOnFailureWhenSpecialThrowable(7, new InterruptedException)
  testOnFailureWhenTimeoutException()
  
}


trait FutureCombinators extends TestBase {

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
    assert(blocking(f.failed, Duration(500, "ms")) == cause)
    done()
  }
  
  def testFailedSuccessAwait(): Unit = once {
    done =>
    val f = future { 0 }
    try {
      blocking(f.failed, Duration(500, "ms"))
      assert(false)
    } catch {
      case nsee: NoSuchElementException => done()
    }
  }
  
  testFailedFailureOnComplete()
  testFailedFailureOnSuccess()
  testFailedSuccessOnComplete()
  testFailedSuccessOnFailure()
  testFailedFailureAwait()
  testFailedSuccessAwait()
  
}


trait Blocking extends TestBase {
  
  def testAwaitSuccess(): Unit = once {
    done =>
    val f = future { 0 }
    blocking(f, Duration(500, "ms"))
    done()
  }
  
  def testAwaitFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    try {
      blocking(f, Duration(500, "ms"))
      assert(false)
    } catch {
      case t =>
        assert(t == cause)
        done()
    }
  }
  
  testAwaitSuccess()
  testAwaitFailure()
  
}


trait Promises extends TestBase {

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
  
}

trait TryEitherExtractor extends TestBase {

  import scala.util.{Try, Success, Failure}

  def testSuccessMatch(): Unit = once {
    done => 
    val thisIsASuccess = Success(42)
    thisIsASuccess match {
      case Success(v) => 
        done()
        assert(v == 42)
      case Failure(e) =>
        done()
        assert(false)
      case other =>
        done()
        assert(false)
    }
  }

  def testRightMatch(): Unit = once {
    done =>
    val thisIsNotASuccess: Right[Throwable, Int] = Right(43)
    thisIsNotASuccess match {
      case Success(v) =>
        done()
        assert(v == 43)
      case Failure(e) =>
        done()
        assert(false)
      case other =>
        done()
        assert(false)
    }
  }

  def testFailureMatch(): Unit = once {
    done =>
    val thisIsAFailure = Failure(new Exception("I'm an exception"))
    thisIsAFailure match {
      case Success(v) =>
        done()
        assert(false)
      case Failure(e) =>
        done()
        assert(e.getMessage == "I'm an exception")
      case other =>
        done()
        assert(false)
    }
  }

  def testLeftMatch(): Unit = once {
    done =>
    val thisIsNotAFailure: Left[Throwable, Int] = Left(new Exception("I'm an exception"))
    thisIsNotAFailure match {
      case Success(v) => 
        done()
        assert(false)
      case Failure(e) =>
        done()
        assert(e.getMessage == "I'm an exception")
      case other =>
        done()
        assert(false)
    }
    
  }

  testSuccessMatch()
  testRightMatch()
  testFailureMatch()
  testLeftMatch()
}

trait CustomExecutionContext extends TestBase {
  import scala.concurrent.{ ExecutionContext, Awaitable }

  def defaultEC = ExecutionContext.defaultExecutionContext

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

    def delegate = ExecutionContext.defaultExecutionContext

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

    override def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T =
      delegate.internalBlockingCall(awaitable, atMost)

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

    // should be onSuccess, but not future body
    assert(count == 1)
  }

  def testKeptPromiseCustomEC(): Unit = {
    val count = countExecs { implicit ec =>
      once { done =>
        val f = Promise.successful(10).future
        f onSuccess {
          case _ =>
            assertEC()
            done()
        }
      }
    }

    // should be onSuccess called once in proper EC
    assert(count == 1)
  }

  def testCallbackChainCustomEC(): Unit = {
    val count = countExecs { implicit ec =>
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
          case Left(t) =>
            try {
              throw new AssertionError("error in test: " + t.getMessage, t)
            } finally {
              done()
            }
          case Right(x) =>
            assertEC()
            assert(x == 14)
            done()
        }
        assertNoEC()
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

object Test
extends App
with FutureCallbacks
with FutureCombinators
with FutureProjections
with Promises
with Exceptions
with TryEitherExtractor
with CustomExecutionContext
{
  System.exit(0)
}




