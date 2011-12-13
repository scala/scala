


import scala.concurrent.{
  Future,
  Promise,
  TimeoutException,
  SyncVar,
  ExecutionException
}



trait TestBase {
  
  def future[T](body: =>T): Future[T]
  
  def promise[T]: Promise[T]
  
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
    f onSuccess { _ =>
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
    f onSuccess { _ =>
      assert(x == 1)
      x = 2
      f onSuccess { _ =>
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
    f onSuccess { _ =>
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
    f onSuccess { _ =>
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
    f onSuccess { _ =>
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
    f onSuccess { _ =>
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
  testOnFailureWhenSpecialThrowable(7, new InterruptedException)
  testOnFailureWhenTimeoutException()
  
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

  // recover: stub
  def testRecoverSuccess(): Unit = once {
    done =>
    done()
  }

  def testRecoverFailure(): Unit = once {
    done =>
    done()
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


trait FutureProjections extends TestBase {
  
}


trait Promises extends TestBase {

  def testSuccess(): Unit = once {
    done =>
    val p = promise[Int]
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


trait Exceptions extends TestBase {
  
}


object Test
extends App
with FutureCallbacks
with FutureCombinators
with FutureProjections
with Promises
with Exceptions
{
  
  def future[T](body: =>T) = scala.concurrent.future(body)
  
  def promise[T] = scala.concurrent.promise[T]

}


