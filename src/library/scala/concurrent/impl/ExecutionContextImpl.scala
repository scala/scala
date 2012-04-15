/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.{Callable, Executor, ExecutorService, Executors, ThreadFactory}
import scala.concurrent.forkjoin._
import scala.concurrent.{ExecutionContext, resolver, Awaitable}
import scala.concurrent.util.{ Duration }



private[scala] class ExecutionContextImpl(es: AnyRef) extends ExecutionContext with Executor {
  import ExecutionContextImpl._
  
  val executorService: AnyRef = if (es eq null) getExecutorService else es
  
  // to ensure that the current execution context thread local is properly set
  def executorsThreadFactory = new ThreadFactory {
    def newThread(r: Runnable) = new Thread(new Runnable {
      override def run() {
        currentExecutionContext.set(ExecutionContextImpl.this)
        r.run()
      }
    })
  }
  
  // to ensure that the current execution context thread local is properly set
  def forkJoinPoolThreadFactory = new ForkJoinPool.ForkJoinWorkerThreadFactory {
    def newThread(fjp: ForkJoinPool) = new ForkJoinWorkerThread(fjp) {
      override def onStart() {
        currentExecutionContext.set(ExecutionContextImpl.this)
      }
    }
  }
  
  def getExecutorService: AnyRef =
    if (scala.util.Properties.isJavaAtLeast("1.6")) {
      val vendor = scala.util.Properties.javaVmVendor
      if ((vendor contains "Oracle") || (vendor contains "Sun") || (vendor contains "Apple"))
        new ForkJoinPool(
          Runtime.getRuntime.availableProcessors(),
          forkJoinPoolThreadFactory,
          null,
          false)
      else
        Executors.newCachedThreadPool(executorsThreadFactory)
    } else Executors.newCachedThreadPool(executorsThreadFactory)

  def execute(runnable: Runnable): Unit = executorService match {
    case fj: ForkJoinPool =>
      Thread.currentThread match {
        case fjw: ForkJoinWorkerThread if fjw.getPool eq fj =>
          val fjtask = runnable match {
            case fjt: ForkJoinTask[_] => fjt
            case _ => ForkJoinTask.adapt(runnable)
          }
          fjtask.fork
        case _ =>
          fj.execute(runnable)
      }
    case executor: Executor =>
      executor execute runnable
  }

  def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T = {
    Future.releaseStack(this)
    
    executorService match {
      case fj: ForkJoinPool =>
        var result: T = null.asInstanceOf[T]
        val managedBlocker = new ForkJoinPool.ManagedBlocker {
          @volatile var isdone = false
          def block() = {
            result = awaitable.result(atMost)(scala.concurrent.Await.canAwaitEvidence)
            isdone = true
            true
          }
          def isReleasable = isdone
        }
        ForkJoinPool.managedBlock(managedBlocker)
        result
      case _ =>
        awaitable.result(atMost)(scala.concurrent.Await.canAwaitEvidence)
    }
  }

  def reportFailure(t: Throwable) = t match {
    // `Error`s are currently wrapped by `resolver`.
    // Also, re-throwing `Error`s here causes an exception handling test to fail.
    //case e: Error => throw e
    case t => t.printStackTrace()
  }

}


private[concurrent] object ExecutionContextImpl {

  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContext] = new ThreadLocal[ExecutionContext] {
    override protected def initialValue = null
  }

}


