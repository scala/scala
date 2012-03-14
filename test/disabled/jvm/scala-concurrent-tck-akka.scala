

import akka.dispatch.{
  Future => future,
  Promise => promise
}
import akka.dispatch.Await.{result => await}

// Duration required for await
import akka.util.Duration
import java.util.concurrent.TimeUnit
import TimeUnit._

import scala.concurrent.{
  TimeoutException,
  SyncVar,
  ExecutionException
}
//import scala.concurrent.future
//import scala.concurrent.promise
//import scala.concurrent.await



trait TestBase {

  implicit val disp = akka.actor.ActorSystem().dispatcher

  def once(body: (() => Unit) => Unit) {
    val sv = new SyncVar[Boolean]
    body(() => sv put true)
    sv.take()
  }
  
}


trait FutureCallbacks extends TestBase {
  
  def testOnSuccess(): Unit = once {
    done =>
    var x = 0
    val f = future {
      x = 1
    }
    f onSuccess { case any =>
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
    f onSuccess { case any =>
      assert(x == 1)
      x = 2
      f onSuccess { case any =>
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
    f onSuccess { case any =>
      assert(false)
    }
  }
  
  def testOnFailure(): Unit = once {
    done =>
    var x = 0
    val f = future[Unit] {
      x = 1
      throw new Exception
    }
    f onSuccess { case any =>
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
    f onSuccess { case any =>
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
    f onSuccess { case any =>
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
//  testOnFailureWhenSpecialThrowable(5, new Error)
//  testOnFailureWhenSpecialThrowable(6, new scala.util.control.ControlThrowable { })
//  testOnFailureWhenSpecialThrowable(7, new InterruptedException)
//  testOnFailureWhenTimeoutException()
  
}


trait FutureCombinators extends TestBase {

  // map: stub
  def testMapSuccess(): Unit = once {
    done =>
    done()
  }

  def testMapFailure(): Unit = once {
    done =>
    done()
  }

  // flatMap: stub
  def testFlatMapSuccess(): Unit = once {
    done =>
    done()
  }

  def testFlatMapFailure(): Unit = once {
    done =>
    done()
  }

  // filter: stub
  def testFilterSuccess(): Unit = once {
    done =>
    done()
  }

  def testFilterFailure(): Unit = once {
    done =>
    done()
  }

  // foreach: stub
  def testForeachSuccess(): Unit = once {
    done =>
    done()
  }

  def testForeachFailure(): Unit = once {
    done =>
    done()
  }

  def testRecoverSuccess(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    } recover {
      case re: RuntimeException =>
        "recovered"
    } onSuccess { case x =>
      done()
      assert(x == "recovered")
    } onFailure { case any =>
      done()
      assert(false)
    }
  }

  def testRecoverFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    } recover {
      case te: TimeoutException => "timeout"
    } onSuccess { case x =>
      done()
      assert(false)
    } onFailure { case any =>
      done()
      assert(any == cause)
    }
  }

  testMapSuccess()
  testMapFailure()
  testFlatMapSuccess()
  testFlatMapFailure()
  testFilterSuccess()
  testFilterFailure()
  testForeachSuccess()
  testForeachFailure()
  testRecoverSuccess()
  testRecoverFailure()

}

/*
trait FutureProjections extends TestBase {
  
  def testFailedFailureOnComplete(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    f.failed onComplete {
      case Right(t) =>
        assert(t == cause)
        done()
      case Left(t) =>
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
      t =>
      assert(t == cause)
      done()
    }
  }
  
  def testFailedSuccessOnComplete(): Unit = once {
    done =>
    val f = future { 0 }
    f.failed onComplete {
      case Right(t) =>
        assert(false)
      case Left(t) =>
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
    assert(await(0, f.failed) == cause)
    done()
  }
  
  def testFailedSuccessAwait(): Unit = once {
    done =>
    val f = future { 0 }
    try {
      println(await(0, f.failed))
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
  //testFailedSuccessAwait()
  
}
*/

trait Blocking extends TestBase {
  
  def testAwaitSuccess(): Unit = once {
    done =>
    val f = future { 0 }
    await(f, Duration(500, "ms"))
    done()
  }
  
  def testAwaitFailure(): Unit = once {
    done =>
    val cause = new RuntimeException
    val f = future {
      throw cause
    }
    try {
      await(f, Duration(500, "ms"))
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

/*
trait Promises extends TestBase {

  def testSuccess(): Unit = once {
    done =>
    val p = promise[Int]()
    val f = p.future
    
    f.onSuccess { x =>
      done()
      assert(x == 5)
    } onFailure { case any =>
      done()
      assert(false)
    }
    
    p.success(5)
  }

  testSuccess()

}
*/

trait Exceptions extends TestBase {
  
}


object Test
extends App
with FutureCallbacks
with FutureCombinators
/*with FutureProjections*/
/*with Promises*/
with Blocking
with Exceptions
{
  System.exit(0)
}


