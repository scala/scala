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
import scala.util.{ Try, Success, Failure }
import scala.concurrent.util.{ Duration }



private[scala] class ExecutionContextImpl(val executorService: AnyRef) extends ExecutionContext {
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

  def blocking[T](body: =>T): T = blocking(body2awaitable(body), Duration.fromNanos(0))

  def blocking[T](awaitable: Awaitable[T], atMost: Duration): T = {
    Future.releaseStack(this)
    
    awaitable.result(atMost)(scala.concurrent.Await.canAwaitEvidence)
  }

  def reportFailure(t: Throwable) = t match {
    case e: Error => throw e // rethrow serious errors
    case t => t.printStackTrace()
  }

}


object ExecutionContextImpl {

  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContextImpl] = new ThreadLocal[ExecutionContextImpl] {
    override protected def initialValue = null
  }

}


