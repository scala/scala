import scala.concurrent.{ executionContext, FutureTimeoutException, ExecutionException, SyncVar }
import executionContext._

object Test extends App {

  def once(body: (() => Unit) => Unit) {
    val sv = new SyncVar[Boolean]
    body(() => sv put true)
    sv.take()
  }

  def output(num: Int, msg: String) {
    println("test" + num + ": " + msg)
  }

  def testOnSuccess(): Unit = once {
    done =>
    val f = future {
      output(1, "hai world")
    }
    f onSuccess { _ =>
      output(1, "kthxbye")
      done()
    }
  }

  def testOnSuccessWhenCompleted(): Unit = once {
    done =>
    val f = future {
      output(2, "hai world")
    }
    f onSuccess { _ =>
      output(2, "awsum thx")
      f onSuccess { _ =>
        output(2, "kthxbye")
        done()
      }
    }
  }

  def testOnSuccessWhenFailed(): Unit = once {
    done =>
    val f = future {
      output(3, "hai world")
      done()
      throw new Exception
    }
    f onSuccess { _ =>
      output(3, "onoes")
    }
  }

  def testOnFailure(): Unit = once {
    done =>
    val f = future {
      output(4, "hai world")
      throw new Exception
    }
    f onSuccess { _ =>
      output(4, "onoes")
      done()
    }
    f onFailure { _ =>
      output(4, "kthxbye")
      done()
    }
  }

  def testOnFailureWhenSpecialThrowable(num: Int, cause: Throwable): Unit = once {
    done =>
    val f = future {
      output(num, "hai world")
      throw cause
    }
    f onSuccess { _ =>
      output(num, "onoes")
      done()
    }
    f onFailure {
      case e: ExecutionException if (e.getCause == cause) =>
        output(num, "kthxbye")
        done()
      case _ =>
        output(num, "onoes")
        done()
    }
  }

  def testOnFailureWhenFutureTimeoutException(): Unit = once {
    done =>
    val f = future {
      output(8, "hai world")
      throw new FutureTimeoutException(null)
    }
    f onSuccess { _ =>
      output(8, "onoes")
      done()
    }
    f onFailure {
      case e: FutureTimeoutException =>
        output(8, "im in yr loop")
        done()
      case other =>
        output(8, "onoes: " + other)
        done()
    }
  }

  testOnSuccess()
  testOnSuccessWhenCompleted()
  testOnSuccessWhenFailed()
  testOnFailure()
  testOnFailureWhenSpecialThrowable(5, new Error)
  testOnFailureWhenSpecialThrowable(6, new scala.util.control.ControlThrowable { })
  testOnFailureWhenSpecialThrowable(7, new InterruptedException)
  testOnFailureWhenFutureTimeoutException()

}
