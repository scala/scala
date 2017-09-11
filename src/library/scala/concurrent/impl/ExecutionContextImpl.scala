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
import java.util.concurrent.Semaphore
import java.util.Collection
import scala.concurrent.{ BlockContext, ExecutionContext, CanAwait, ExecutionContextExecutor, ExecutionContextExecutorService }


private[scala] class ExecutionContextImpl private[impl] (val executor: Executor, val reporter: Throwable => Unit) extends ExecutionContextExecutor {
  require(executor ne null, "Executor must not be null")
  override def execute(runnable: Runnable) = executor execute runnable
  override def reportFailure(t: Throwable) = reporter(t)
}


private[concurrent] object ExecutionContextImpl {
  // Implement BlockContext on FJP threads
  final class DefaultThreadFactory(
    daemonic: Boolean,
    maxExtraThreads: Int,
    prefix: String,
    uncaught: Thread.UncaughtExceptionHandler)
    extends Semaphore(maxExtraThreads) with ThreadFactory with ForkJoinPool.ForkJoinWorkerThreadFactory {

    require(prefix ne null, "DefaultThreadFactory.prefix must be non null")
    require(maxExtraThreads >= 0, "DefaultThreadFactory.maxExtraThreads must be greater than or equal to 0")

    override def toString: String =
      s"DefaultThreadFactory(daemonic = $daemonic, maxExtraThreads = $maxExtraThreads, prefix = $prefix, uncaught = $uncaught)"

    def wire[T <: Thread](thread: T): T = {
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler(uncaught)
      thread.setName(prefix + "-" + thread.getId())
      thread
    }

    private[this] final def shouldAllocateUnlimited(): Boolean =
      Thread.currentThread match {
        case w: WorkerThread if w.isBlocking => false
        case _ => true
      }

    override def newThread(runnable: Runnable): Thread =
      if (shouldAllocateUnlimited()) wire(new Thread(runnable))
      else if (tryAcquire(1)) wire(new Thread(new Runnable {
          def run(): Unit = try runnable.run() finally release()
        }))
      else null

    override def newThread(fjp: ForkJoinPool): ForkJoinWorkerThread =
      if (shouldAllocateUnlimited()) wire(new WorkerThread(fjp, null))
      else if (tryAcquire(1)) wire(new WorkerThread(fjp, this))
      else null

      final class WorkerThread(final val fjp: ForkJoinPool, final val limiter: Semaphore)
      extends ForkJoinWorkerThread(fjp) with BlockContext {
        @volatile var isBlocking: Boolean = false
        final override def toString: String = s"Worker(isBlocking = $isBlocking, limited = ${limiter ne null})"
        final override def onTermination(exception: Throwable): Unit = if (limiter ne null) limiter.release()
        final override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T = {
            isBlocking = true
            try {
              if ((limiter ne null) && limiter.availablePermits < 1) {
                BlockContext.defaultBlockContext.blockOn(thunk)
              } else {
                val b = new Block(thunk)
                ForkJoinPool.managedBlock(b)
                b.result
              }
            } finally {
              isBlocking = false
            }
          }
      }
    final class Block[T](thunk: =>T) extends ForkJoinPool.ManagedBlocker {
      @volatile var isReleasable: Boolean = _ // TODO: validate whether @volatile is required or not
      var result: T = null.asInstanceOf[T]
      override def block(): Boolean = {
        if (!isReleasable)
          result = try thunk finally { isReleasable = true }

        true
      }
    }
  }

  def createDefaultExecutorService(reporter: Throwable => Unit): ExecutorService = {
    def getInt(name: String, default: String) = (try System.getProperty(name, default) catch {
      case e: SecurityException => default
    }) match {
      case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
      case other => other.toInt
    }

    def range(floor: Int, desired: Int, ceiling: Int): Int = scala.math.min(scala.math.max(floor, desired), ceiling)

    val desiredParallelism = range(
      getInt("scala.concurrent.context.minThreads", "1"),
      getInt("scala.concurrent.context.numThreads", "x1"),
      getInt("scala.concurrent.context.maxThreads", "x1"))

    // The thread factory must provide additional threads to support managed blocking.
    val extraThreads = getInt("scala.concurrent.context.maxExtraThreads", "256")

    val uncaughtExceptionHandler: Thread.UncaughtExceptionHandler = new Thread.UncaughtExceptionHandler {
      override def uncaughtException(thread: Thread, cause: Throwable): Unit = reporter(cause)
    }

    val threadFactory = new ExecutionContextImpl.DefaultThreadFactory(daemonic = true,
                                                                      maxExtraThreads = extraThreads,
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

