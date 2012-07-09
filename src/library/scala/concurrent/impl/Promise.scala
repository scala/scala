/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.TimeUnit.{ NANOSECONDS, MILLISECONDS }
import scala.concurrent.{ Awaitable, ExecutionContext, blocking, CanAwait, OnCompleteRunnable, TimeoutException, ExecutionException }
//import scala.util.continuations._
import scala.concurrent.util.Duration
import scala.util
import scala.annotation.tailrec
import scala.util.control.NonFatal
//import scala.concurrent.NonDeterministic



private[concurrent] trait Promise[T] extends scala.concurrent.Promise[T] with Future[T] {
  def future: this.type = this
}

private class CallbackRunnable[T](val executor: ExecutionContext, val onComplete: (Either[Throwable, T]) => Any) extends Runnable with OnCompleteRunnable {
  // must be filled in before running it
  var value: Either[Throwable, T] = null

  override def run() = {
    require(value ne null) // must set value to non-null before running!
    try onComplete(value) catch { case NonFatal(e) => executor reportFailure e }
  }

  def executeWithValue(v: Either[Throwable, T]): Unit = {
    require(value eq null) // can't complete it twice
    value = v
    executor.execute(this)
  }
}

object Promise {

  private def resolveEither[T](source: Either[Throwable, T]): Either[Throwable, T] = source match {
    case Left(t) => resolver(t)
    case _       => source
  }
  
  private def resolver[T](throwable: Throwable): Either[Throwable, T] = throwable match {
    case t: scala.runtime.NonLocalReturnControl[_] => Right(t.value.asInstanceOf[T])
    case t: scala.util.control.ControlThrowable    => Left(new ExecutionException("Boxed ControlThrowable", t))
    case t: InterruptedException                   => Left(new ExecutionException("Boxed InterruptedException", t))
    case e: Error                                  => Left(new ExecutionException("Boxed Error", e))
    case t                                         => Left(t)
  }
  
  /** Default promise implementation.
   */
  class DefaultPromise[T] extends AbstractPromise with Promise[T] { self =>
    updateState(null, Nil) // Start at "No callbacks" //FIXME switch to Unsafe instead of ARFU
    
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
      //FIXME do not do this if there'll be no waiting
      awaitUnsafe(if (atMost.isFinite) atMost.toNanos else Long.MaxValue)
    }

    @throws(classOf[TimeoutException])
    def ready(atMost: Duration)(implicit permit: CanAwait): this.type =
      if (isCompleted || tryAwait(atMost)) this
      else throw new TimeoutException("Futures timed out after [" + atMost.toMillis + "] milliseconds")

    @throws(classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): T =
      ready(atMost).value.get match {
        case Left(e)  => throw e
        case Right(r) => r
      }

    def value: Option[Either[Throwable, T]] = getState match {
      case c: Either[_, _] => Some(c.asInstanceOf[Either[Throwable, T]])
      case _               => None
    }

    override def isCompleted(): Boolean = getState match { // Cheaper than boxing result into Option due to "def value"
      case _: Either[_, _] => true
      case _               => false
    }

    def tryComplete(value: Either[Throwable, T]): Boolean = {
      val resolved = resolveEither(value)
      (try {
        @tailrec
        def tryComplete(v: Either[Throwable, T]): List[CallbackRunnable[T]] = {
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

    def onComplete[U](func: Either[Throwable, T] => U)(implicit executor: ExecutionContext): Unit = {
      val runnable = new CallbackRunnable[T](executor, func)

      @tailrec //Tries to add the callback, if already completed, it dispatches the callback to be executed
      def dispatchOrAddCallback(): Unit =
        getState match {
          case r: Either[_, _]    => runnable.executeWithValue(r.asInstanceOf[Either[Throwable, T]])
          case listeners: List[_] => if (updateState(listeners, runnable :: listeners)) () else dispatchOrAddCallback()
        }
      dispatchOrAddCallback()
    }
  }

  /** An already completed Future is given its result at creation.
   *
   *  Useful in Future-composition when a value to contribute is already available.
   */
  final class KeptPromise[T](suppliedValue: Either[Throwable, T]) extends Promise[T] {

    val value = Some(resolveEither(suppliedValue))

    override def isCompleted(): Boolean = true

    def tryComplete(value: Either[Throwable, T]): Boolean = false

    def onComplete[U](func: Either[Throwable, T] => U)(implicit executor: ExecutionContext): Unit = {
      val completedAs = value.get
      (new CallbackRunnable(executor, func)).executeWithValue(completedAs)
    }

    def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

    def result(atMost: Duration)(implicit permit: CanAwait): T = value.get match {
      case Left(e)  => throw e
      case Right(r) => r
    }
  }

}
