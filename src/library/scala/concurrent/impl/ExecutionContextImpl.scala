/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl

import java.util.concurrent.{ ForkJoinPool, ForkJoinWorkerThread, ForkJoinTask, Callable, Executor, ExecutorService, ThreadFactory, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger
import java.util.Collection
import scala.concurrent.{ BlockContext, ExecutionContext, CanAwait, ExecutionContextExecutor, ExecutionContextExecutorService }
import scala.annotation.tailrec


private[scala] class ExecutionContextImpl private[impl] (val executor: Executor, val reporter: Throwable => Unit) extends ExecutionContextExecutor {
  require(executor ne null, "Executor must not be null")
  override def execute(runnable: Runnable) = executor execute runnable
  override def reportFailure(t: Throwable) = reporter(t)
}


private[concurrent] object ExecutionContextImpl {

  // Implement BlockContext on FJP threads
  final class DefaultThreadFactory(
    daemonic: Boolean,
    maxThreads: Int,
    prefix: String,
    uncaught: Thread.UncaughtExceptionHandler) extends ThreadFactory with ForkJoinPool.ForkJoinWorkerThreadFactory {

    require(prefix ne null, "DefaultThreadFactory.prefix must be non null")
    require(maxThreads > 0, "DefaultThreadFactory.maxThreads must be greater than 0")

    private final val currentNumberOfThreads = new AtomicInteger(0)

    @tailrec private final def reserveThread(): Boolean = currentNumberOfThreads.get() match {
      case `maxThreads` | Int.`MaxValue` => false
      case other => currentNumberOfThreads.compareAndSet(other, other + 1) || reserveThread()
    }

    @tailrec private final def deregisterThread(): Boolean = currentNumberOfThreads.get() match {
      case 0 => false
      case other => currentNumberOfThreads.compareAndSet(other, other - 1) || deregisterThread()
    }

    def wire[T <: Thread](thread: T): T = {
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler(uncaught)
      thread.setName(prefix + "-" + thread.getId())
      thread
    }

    // As per ThreadFactory contract newThread should return `null` if cannot create new thread.
    def newThread(runnable: Runnable): Thread =
      if (reserveThread())
        wire(new Thread(new Runnable {
          // We have to decrement the current thread count when the thread exits
          override def run() = try runnable.run() finally deregisterThread()
        })) else null

    def newThread(fjp: ForkJoinPool): ForkJoinWorkerThread =
      if (reserveThread()) {
        wire(new ForkJoinWorkerThread(fjp) with BlockContext {
          // We have to decrement the current thread count when the thread exits
          final override def onTermination(exception: Throwable): Unit = deregisterThread()
          final override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T = {
            var result: T = null.asInstanceOf[T]
            ForkJoinPool.managedBlock(new ForkJoinPool.ManagedBlocker {
              @volatile var isdone = false
              override def block(): Boolean = {
                result = try {
                    // When we block, switch out the BlockContext temporarily so that nested blocking does not created N new Threads
                    BlockContext.withBlockContext(BlockContext.defaultBlockContext) { thunk }
                  } finally {
                    isdone = true
                  }

                true
              }
              override def isReleasable = isdone
            })
            result
          }
        })
      } else null
  }

  def createDefaultExecutorService(reporter: Throwable => Unit): ExecutorService = {
    def getInt(name: String, default: String) = (try System.getProperty(name, default) catch {
      case e: SecurityException => default
    }) match {
      case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
      case other => other.toInt
    }

    def range(floor: Int, desired: Int, ceiling: Int) = scala.math.min(scala.math.max(floor, desired), ceiling)
    val numThreads = getInt("scala.concurrent.context.numThreads", "x1")
    // The hard limit on the number of active threads that the thread factory will produce
    // SI-8955 Deadlocks can happen if maxNoOfThreads is too low, although we're currently not sure
    //         about what the exact threshhold is. numThreads + 256 is conservatively high.
    val maxNoOfThreads = getInt("scala.concurrent.context.maxThreads", "x1")

    val desiredParallelism = range(
      getInt("scala.concurrent.context.minThreads", "1"),
      numThreads,
      maxNoOfThreads)

    // The thread factory must provide additional threads to support managed blocking.
    val maxExtraThreads = getInt("scala.concurrent.context.maxExtraThreads", "256")

    val uncaughtExceptionHandler: Thread.UncaughtExceptionHandler = new Thread.UncaughtExceptionHandler {
      override def uncaughtException(thread: Thread, cause: Throwable): Unit = reporter(cause)
    }

    val threadFactory = new ExecutionContextImpl.DefaultThreadFactory(daemonic = true,
                                                                      maxThreads = maxNoOfThreads + maxExtraThreads,
                                                                      prefix = "scala-execution-context-global",
                                                                      uncaught = uncaughtExceptionHandler)

    new ForkJoinPool(desiredParallelism, threadFactory, uncaughtExceptionHandler, true) {
      override def execute(runnable: Runnable): Unit = {
        val fjt: ForkJoinTask[_] = runnable match {
          case t: ForkJoinTask[_] => t
          case r                  => new ExecutionContextImpl.AdaptedForkJoinTask(r)
        }
        Thread.currentThread match {
          case fjw: ForkJoinWorkerThread if fjw.getPool eq this => fjt.fork()
          case _                                                => super.execute(fjt)
        }
      }
    }
  }

  final class AdaptedForkJoinTask(runnable: Runnable) extends ForkJoinTask[Unit] {
    final override def setRawResult(u: Unit): Unit = ()
    final override def getRawResult(): Unit = ()
    final override def exec(): Boolean = try { runnable.run(); true } catch {
      case anything: Throwable =>
        val t = Thread.currentThread
        t.getUncaughtExceptionHandler match {
          case null =>
          case some => some.uncaughtException(t, anything)
        }
        throw anything
    }
  }

  def fromExecutor(e: Executor, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextImpl =
    new ExecutionContextImpl(Option(e).getOrElse(createDefaultExecutorService(reporter)), reporter)

  def fromExecutorService(es: ExecutorService, reporter: Throwable => Unit = ExecutionContext.defaultReporter):
    ExecutionContextImpl with ExecutionContextExecutorService = {
    new ExecutionContextImpl(Option(es).getOrElse(createDefaultExecutorService(reporter)), reporter)
      with ExecutionContextExecutorService {
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
}


