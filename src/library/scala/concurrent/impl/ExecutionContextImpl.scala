/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.{ Callable, Executor, ExecutorService, Executors, ThreadFactory, TimeUnit }
import java.util.Collection
import scala.concurrent.forkjoin._
import scala.concurrent.{ ExecutionContext, Awaitable }
import scala.concurrent.util.Duration
import scala.util.control.NonFatal



private[scala] class ExecutionContextImpl private[impl] (es: Executor, reporter: Throwable => Unit) extends ExecutionContext with Executor {

  val executor: Executor = es match {
    case null => createExecutorService
    case some => some
  }
  
  // to ensure that the current execution context thread local is properly set
  def executorsThreadFactory = new ThreadFactory {
    def newThread(r: Runnable) = new Thread(new Runnable {
      override def run() {
        scala.concurrent.currentExecutionContext.set(ExecutionContextImpl.this)
        r.run()
      }
    })
  }
  
  // to ensure that the current execution context thread local is properly set
  def forkJoinPoolThreadFactory = new ForkJoinPool.ForkJoinWorkerThreadFactory {
    def newThread(fjp: ForkJoinPool) = new ForkJoinWorkerThread(fjp) {
      override def onStart() {
        scala.concurrent.currentExecutionContext.set(ExecutionContextImpl.this)
      }
    }
  }

  def createExecutorService: ExecutorService = try {
    def getInt(name: String, f: String => Int): Int =
      try f(System.getProperty(name)) catch { case e: Exception => Runtime.getRuntime.availableProcessors }
    def range(floor: Int, desired: Int, ceiling: Int): Int =
      if (ceiling < floor) range(ceiling, desired, floor) else scala.math.min(scala.math.max(desired, floor), ceiling)
    
    new ForkJoinPool(
      range(
        getInt("scala.concurrent.ec.minThreads", _.toInt),
        getInt("scala.concurrent.ec.numThreads", {
          case null | "" => Runtime.getRuntime.availableProcessors
          case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
          case other => other.toInt
        }),
        getInt("scala.concurrent.ec.maxThreads", _.toInt)
      ),
      forkJoinPoolThreadFactory,
      null, //FIXME we should have an UncaughtExceptionHandler, see what Akka does
      true) //FIXME I really think this should be async...
  } catch {
    case NonFatal(t) =>
      System.err.println("Failed to create ForkJoinPool for the default ExecutionContext, falling back to Executors.newCachedThreadPool")
      t.printStackTrace(System.err)
      Executors.newCachedThreadPool(executorsThreadFactory) //FIXME use the same desired parallelism here too?
  }

  def execute(runnable: Runnable): Unit = executor match {
    case fj: ForkJoinPool =>
      Thread.currentThread match {
        case fjw: ForkJoinWorkerThread if fjw.getPool eq fj =>
          (runnable match {
            case fjt: ForkJoinTask[_] => fjt
            case _ => ForkJoinTask.adapt(runnable)
          }).fork
        case _ => fj.execute(runnable)
      }
    case generic => generic execute runnable
  }

  def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T = {
    Future.releaseStack(this)
    
    executor match {
      case fj: ForkJoinPool =>
        var result: T = null.asInstanceOf[T]
        ForkJoinPool.managedBlock(new ForkJoinPool.ManagedBlocker { 
          @volatile var isdone = false
          def block(): Boolean = {
            result = awaitable.result(atMost)(scala.concurrent.Await.canAwaitEvidence) // FIXME what happens if there's an exception thrown here?
            isdone = true
            true
          }
          def isReleasable = isdone
        })
        result
      case _ =>
        awaitable.result(atMost)(scala.concurrent.Await.canAwaitEvidence)
    }
  }

  def reportFailure(t: Throwable) = reporter(t)
}


private[concurrent] object ExecutionContextImpl {

  def fromExecutor(e: Executor, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextImpl = new ExecutionContextImpl(e, reporter)
  def fromExecutorService(es: ExecutorService, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextImpl with ExecutorService =
    new ExecutionContextImpl(es, reporter) with ExecutorService {
      final def asExecutorService: ExecutorService = executor.asInstanceOf[ExecutorService]
      override def execute(command: Runnable) = executor.execute(command)
      override def shutdown() { asExecutorService.shutdown() }
      override def shutdownNow() = asExecutorService.shutdownNow()
      override def isShutdown = asExecutorService.isShutdown
      override def isTerminated = asExecutorService.isTerminated
      override def awaitTermination(l: Long, timeUnit: TimeUnit) = asExecutorService.awaitTermination(l, timeUnit)
      override def submit[T](callable: Callable[T]) = asExecutorService.submit(callable)
      override def submit[T](runnable: Runnable, t: T) = asExecutorService.submit(runnable, t)
      override def submit(runnable: Runnable) = asExecutorService.submit(runnable)
      override def invokeAll[T](callables: Collection[_ <: Callable[T]]) = asExecutorService.invokeAll(callables)
      override def invokeAll[T](callables: Collection[_ <: Callable[T]], l: Long, timeUnit: TimeUnit) = asExecutorService.invokeAll(callables, l, timeUnit)
      override def invokeAny[T](callables: Collection[_ <: Callable[T]]) = asExecutorService.invokeAny(callables)
      override def invokeAny[T](callables: Collection[_ <: Callable[T]], l: Long, timeUnit: TimeUnit) = asExecutorService.invokeAny(callables, l, timeUnit)
    }
}


