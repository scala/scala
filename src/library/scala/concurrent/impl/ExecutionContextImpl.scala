/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.{Callable, ExecutorService}
import scala.concurrent.forkjoin._
import scala.concurrent.{ExecutionContext, resolver, Awaitable, body2awaitable}
import scala.util.{ Duration, Try, Success, Failure }
import scala.collection.mutable.Stack



class ExecutionContextImpl(val executorService: AnyRef) extends ExecutionContext {
  import ExecutionContextImpl._

  def execute(runnable: Runnable): Unit = executorService match {
    case fj: ForkJoinPool =>
      if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) {
        val fjtask = ForkJoinTask.adapt(runnable)
        fjtask.fork
      } else {
        fj.execute(runnable)
      }
    case executorService: ExecutorService =>
      executorService execute runnable
  }

  def execute[U](body: () => U): Unit = execute(new Runnable {
    def run() = body()
  })

  def promise[T]: Promise[T] = new Promise.DefaultPromise[T]()(this)

  def future[T](body: =>T): Future[T] = {
    val p = promise[T]

    dispatchFuture {
      () =>
      p complete {
        try {
          Success(body)
        } catch {
          case e => resolver(e)
        }
      }
    }

    p.future
  }

  def blocking[T](atMost: Duration)(body: =>T): T = blocking(body2awaitable(body), atMost)

  def blocking[T](awaitable: Awaitable[T], atMost: Duration): T = {
    currentExecutionContext.get match {
      case null => awaitable.await(atMost)(null) // outside - TODO - fix timeout case
      case x => x.blockingCall(awaitable) // inside an execution context thread
    }
  }

  def reportFailure(t: Throwable) = t match {
    case e: Error => throw e // rethrow serious errors
    case t => t.printStackTrace()
  }

  /** Only callable from the tasks running on the same execution context. */
  private def blockingCall[T](body: Awaitable[T]): T = {
    releaseStack()

    // TODO see what to do with timeout
    body.await(Duration.fromNanos(0))(CanAwaitEvidence)
  }

  // an optimization for batching futures
  // TODO we should replace this with a public queue,
  // so that it can be stolen from
  // OR: a push to the local task queue should be so cheap that this is
  // not even needed, but stealing is still possible
  private val _taskStack = new ThreadLocal[Stack[() => Unit]]()

  private def releaseStack(): Unit =
    _taskStack.get match {
      case stack if (stack ne null) && stack.nonEmpty =>
        val tasks = stack.elems
        stack.clear()
        _taskStack.remove()
        dispatchFuture(() => _taskStack.get.elems = tasks, true)
      case null =>
        // do nothing - there is no local batching stack anymore
      case _ =>
        _taskStack.remove()
    }

  private[impl] def dispatchFuture(task: () => Unit, force: Boolean = false): Unit =
    _taskStack.get match {
      case stack if (stack ne null) && !force => stack push task
      case _ => this.execute(
        new Runnable {
          def run() {
            try {
              val taskStack = Stack[() => Unit](task)
              _taskStack set taskStack
              while (taskStack.nonEmpty) {
                val next = taskStack.pop()
                try {
                  next.apply()
                } catch {
                  case e =>
                    // TODO catching all and continue isn't good for OOME
                    reportFailure(e)
                }
              }
            } finally {
              _taskStack.remove()
            }
          }
        }
      )
    }

}


object ExecutionContextImpl {

  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContextImpl] = new ThreadLocal[ExecutionContextImpl] {
    override protected def initialValue = null
  }

}


