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
import scala.concurrent.{ExecutionContext, resolver, Awaitable, body2awaitable}
import scala.concurrent.util.{ Duration }



private[scala] class ExecutionContextImpl(es: AnyRef /* AnyRef? */) extends ExecutionContext with Executor {
  import ExecutionContextImpl._
  
  val executorService: AnyRef = if (es eq null) getExecutorService else es
  
  // to ensure that the current execution context thread local is properly set
  //FIXME Not configurable
  def executorsThreadFactory = new ThreadFactory {
    def newThread(r: Runnable) = new Thread(new Runnable {
      override def run() {
        currentExecutionContext.set(ExecutionContextImpl.this)
        r.run()
        //FIXME Doesn't clear out the currentExecutionContext
      }
    })
  }
  
  // to ensure that the current execution context thread local is properly set
  def forkJoinPoolThreadFactory = new ForkJoinPool.ForkJoinWorkerThreadFactory {
    def newThread(fjp: ForkJoinPool) = new ForkJoinWorkerThread(fjp) {
      override def onStart() {
        currentExecutionContext.set(ExecutionContextImpl.this)//FIXME Doesn't clear out the currentExecutionContext
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
          false) //FIXME ForkJoinPool Should most definitely still be an ExecutorService, unless someone can put forth convincing arguments to the contrary.
      else
        Executors.newCachedThreadPool(executorsThreadFactory)
    } else Executors.newCachedThreadPool(executorsThreadFactory)

  def execute(runnable: Runnable): Unit = executorService match {
    case fj: ForkJoinPool =>
      if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) { // Needs to check the the FJWT's pool is the same instance as "fj", no?
        val fjtask = ForkJoinTask.adapt(runnable) //Shouldn't adapt if it's already a FJT
        fjtask.fork
      } else {
        fj.execute(runnable)
      }
    case executor: Executor =>
      executor execute runnable
  }

  def execute[U](body: () => U): Unit = execute(new Runnable { // Should be dropped in favor of an implicit/explicit from Function0 to Runnable
    def run() = body()
  })

  def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T = {
    Future.releaseStack(this)
    
    awaitable.result(atMost)(scala.concurrent.Await.canAwaitEvidence)
  }

  def reportFailure(t: Throwable) = t match {
    case e: Error => throw e // rethrow serious errors
    case t => t.printStackTrace()
  }

}


object ExecutionContextImpl { // Shouldn't this be private[concurrent]?

  private[concurrent] def currentExecutionContext: ThreadLocal[ExecutionContext] = new ThreadLocal[ExecutionContext] {
    override protected def initialValue = null
  }

}


