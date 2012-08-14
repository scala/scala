/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.TimeUnit.NANOSECONDS
import scala.concurrent.{ ExecutionContext, CanAwait, OnCompleteRunnable, TimeoutException, ExecutionException }
import scala.concurrent.util.Duration
import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{ Try, Success, Failure }


private[concurrent] trait Promise[T] extends scala.concurrent.Promise[T] with scala.concurrent.Future[T] {
  def future: this.type = this
}

/* Precondition: `executor` is prepared, i.e., `executor` has been returned from invocation of `prepare` on some other `ExecutionContext`.
 */
private class CallbackRunnable[T](val executor: ExecutionContext, val onComplete: Try[T] => Any) extends Runnable with OnCompleteRunnable {
  // must be filled in before running it
  var value: Try[T] = null

  override def run() = {
    require(value ne null) // must set value to non-null before running!
    try onComplete(value) catch { case NonFatal(e) => executor reportFailure e }
  }

  def executeWithValue(v: Try[T]): Unit = {
    require(value eq null) // can't complete it twice
    value = v
    // Note that we cannot prepare the ExecutionContext at this point, since we might
    // already be running on a different thread!
    executor.execute(this)
  }
}

private[concurrent] object Promise {

  private def resolveTry[T](source: Try[T]): Try[T] = source match {
    case Failure(t) => resolver(t)
    case _          => source
  }
  
  private def resolver[T](throwable: Throwable): Try[T] = throwable match {
    case t: scala.runtime.NonLocalReturnControl[_] => Success(t.value.asInstanceOf[T])
    case t: scala.util.control.ControlThrowable    => Failure(new ExecutionException("Boxed ControlThrowable", t))
    case t: InterruptedException                   => Failure(new ExecutionException("Boxed InterruptedException", t))
    case e: Error                                  => Failure(new ExecutionException("Boxed Error", e))
    case t                                         => Failure(t)
  }
  
  /** Default promise implementation.
   */
  class DefaultPromise[T] extends AbstractPromise with Promise[T] { self =>
    updateState(null, Nil) // Start at "No callbacks"
    
    protected final def tryAwait(atMost: Duration): Boolean = {
      @tailrec
      def awaitUnsafe(waitTimeNanos: Long): Boolean = {
        if (value.isEmpty && waitTimeNanos > 0) {
          val ms = NANOSECONDS.toMillis(waitTimeNanos)
          val ns = (waitTimeNanos % 1000000l).toInt // as per object.wait spec
          val start = System.nanoTime()
          try {
            synchronized {
              if (!isCompleted) wait(ms, ns) // previously - this was a `while`, ending up in an infinite loop
            }
          } catch {
            case e: InterruptedException =>
          }

          awaitUnsafe(waitTimeNanos - (System.nanoTime() - start))
        } else
          isCompleted
      }
      awaitUnsafe(if (atMost.isFinite) atMost.toNanos else Long.MaxValue)
    }

    @throws(classOf[TimeoutException])
    def ready(atMost: Duration)(implicit permit: CanAwait): this.type =
      if (isCompleted || tryAwait(atMost)) this
      else throw new TimeoutException("Futures timed out after [" + atMost.toMillis + "] milliseconds")

    @throws(classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): T =
      ready(atMost).value.get match {
        case Failure(e)  => throw e
        case Success(r) => r
      }

    def value: Option[Try[T]] = getState match {
      case c: Try[_] => Some(c.asInstanceOf[Try[T]])
      case _ => None
    }

    override def isCompleted(): Boolean = getState match { // Cheaper than boxing result into Option due to "def value"
      case _: Try[_] => true
      case _ => false
    }

    def tryComplete(value: Try[T]): Boolean = {
      val resolved = resolveTry(value)
      (try {
        @tailrec
        def tryComplete(v: Try[T]): List[CallbackRunnable[T]] = {
          getState match {
            case raw: List[_] =>
              val cur = raw.asInstanceOf[List[CallbackRunnable[T]]]
              if (updateState(cur, v)) cur else tryComplete(v)
            case _ => null
          }
        }
        tryComplete(resolved)
      } finally {
        synchronized { notifyAll() } //Notify any evil blockers
      }) match {
        case null             => false
        case rs if rs.isEmpty => true
        case rs               => rs.foreach(r => r.executeWithValue(resolved)); true
      }
    }

    def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext): Unit = {
      val preparedEC = executor.prepare
      val runnable = new CallbackRunnable[T](preparedEC, func)

      @tailrec //Tries to add the callback, if already completed, it dispatches the callback to be executed
      def dispatchOrAddCallback(): Unit =
        getState match {
          case r: Try[_]          => runnable.executeWithValue(r.asInstanceOf[Try[T]])
          case listeners: List[_] => if (updateState(listeners, runnable :: listeners)) () else dispatchOrAddCallback()
        }
      dispatchOrAddCallback()
    }
  }

  /** An already completed Future is given its result at creation.
   *
   *  Useful in Future-composition when a value to contribute is already available.
   */
  final class KeptPromise[T](suppliedValue: Try[T]) extends Promise[T] {

    val value = Some(resolveTry(suppliedValue))

    override def isCompleted(): Boolean = true

    def tryComplete(value: Try[T]): Boolean = false

    def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext): Unit = {
      val completedAs = value.get
      val preparedEC = executor.prepare
      (new CallbackRunnable(preparedEC, func)).executeWithValue(completedAs)
    }

    def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

    def result(atMost: Duration)(implicit permit: CanAwait): T = value.get.get
  }

}
