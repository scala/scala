/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.impl

import java.util.concurrent.{ Semaphore, ForkJoinPool, ForkJoinWorkerThread, Callable, Executor, ExecutorService, ThreadFactory, TimeUnit }
import java.util.Collection
import scala.concurrent.{ BlockContext, ExecutionContext, CanAwait, ExecutionContextExecutor, ExecutionContextExecutorService }

private[scala] class ExecutionContextImpl private[impl] (final val executor: Executor, final val reporter: Throwable => Unit) extends ExecutionContextExecutor {
  require(executor ne null, "Executor must not be null")
  override final def execute(runnable: Runnable): Unit = executor execute runnable
  override final def reportFailure(t: Throwable): Unit = reporter(t)
}

private[concurrent] object ExecutionContextImpl {

  final class DefaultThreadFactory(
    final val daemonic: Boolean,
    final val maxBlockers: Int,
    final val prefix: String,
    final val uncaught: Thread.UncaughtExceptionHandler) extends ThreadFactory with ForkJoinPool.ForkJoinWorkerThreadFactory {

    require(prefix ne null, "DefaultThreadFactory.prefix must be non null")
    require(maxBlockers >= 0, "DefaultThreadFactory.maxBlockers must be greater-or-equal-to 0")

    private final val blockerPermits = new Semaphore(maxBlockers)

    @annotation.nowarn("cat=deprecation")
    def wire[T <: Thread](thread: T): T = {
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler(uncaught)
      thread.setName(prefix + "-" + thread.getId())
      thread
    }

    def newThread(runnable: Runnable): Thread = wire(new Thread(runnable))

    def newThread(fjp: ForkJoinPool): ForkJoinWorkerThread =
      wire(new ForkJoinWorkerThread(fjp) with BlockContext {
        private[this] final var isBlocked: Boolean = false // This is only ever read & written if this thread is the current thread
        final override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T =
          if ((Thread.currentThread eq this) && !isBlocked && blockerPermits.tryAcquire()) {
            try {
              val b: ForkJoinPool.ManagedBlocker with (() => T) =
                new ForkJoinPool.ManagedBlocker with (() => T) {
                  private[this] final var result: T = null.asInstanceOf[T]
                  private[this] final var done: Boolean = false
                  final override def block(): Boolean = {
                    if (!done) {
                      result = thunk // If this throws then it will stop blocking.
                      done = true
                    }

                    isReleasable
                  }

                  final override def isReleasable = done
                  final override def apply(): T = result
                }
              isBlocked = true
              ForkJoinPool.managedBlock(b)
              b()
            } finally {
              isBlocked = false
              blockerPermits.release()
            }
          } else thunk // Unmanaged blocking
      })
  }

  def createDefaultExecutorService(reporter: Throwable => Unit): ExecutionContextExecutorService = {
    def getInt(name: String, default: String) = (try System.getProperty(name, default) catch {
      case e: SecurityException => default
    }) match {
      case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
      case other => other.toInt
    }

    val desiredParallelism = // A range between min and max given num
      scala.math.min(
        scala.math.max(
          getInt("scala.concurrent.context.minThreads", "1"),
          getInt("scala.concurrent.context.numThreads", "x1")),
          getInt("scala.concurrent.context.maxThreads", "x1")
        )

    val threadFactory = new DefaultThreadFactory(daemonic = true,
                                                 maxBlockers = getInt("scala.concurrent.context.maxExtraThreads", "256"),
                                                 prefix = "scala-execution-context-global",
                                                 uncaught = (thread: Thread, cause: Throwable) => reporter(cause))

    new ForkJoinPool(desiredParallelism, threadFactory, threadFactory.uncaught, true) with ExecutionContextExecutorService {
      final override def reportFailure(cause: Throwable): Unit =
        getUncaughtExceptionHandler() match {
          case null =>
          case some => some.uncaughtException(Thread.currentThread, cause)
        }
    }
  }

  def fromExecutor(e: Executor, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextExecutor =
    e match {
      case null => createDefaultExecutorService(reporter)
      case some => new ExecutionContextImpl(some, reporter)
    }

  def fromExecutorService(es: ExecutorService, reporter: Throwable => Unit = ExecutionContext.defaultReporter):
    ExecutionContextExecutorService = es match {
      case null => createDefaultExecutorService(reporter)
      case some =>
        new ExecutionContextImpl(some, reporter) with ExecutionContextExecutorService {
            private[this] final def asExecutorService: ExecutorService = executor.asInstanceOf[ExecutorService]
            final override def shutdown() = asExecutorService.shutdown()
            final override def shutdownNow() = asExecutorService.shutdownNow()
            final override def isShutdown = asExecutorService.isShutdown
            final override def isTerminated = asExecutorService.isTerminated
            final override def awaitTermination(l: Long, timeUnit: TimeUnit) = asExecutorService.awaitTermination(l, timeUnit)
            final override def submit[T](callable: Callable[T]) = asExecutorService.submit(callable)
            final override def submit[T](runnable: Runnable, t: T) = asExecutorService.submit(runnable, t)
            final override def submit(runnable: Runnable) = asExecutorService.submit(runnable)
            final override def invokeAll[T](callables: Collection[_ <: Callable[T]]) = asExecutorService.invokeAll(callables)
            final override def invokeAll[T](callables: Collection[_ <: Callable[T]], l: Long, timeUnit: TimeUnit) = asExecutorService.invokeAll(callables, l, timeUnit)
            final override def invokeAny[T](callables: Collection[_ <: Callable[T]]) = asExecutorService.invokeAny(callables)
            final override def invokeAny[T](callables: Collection[_ <: Callable[T]], l: Long, timeUnit: TimeUnit) = asExecutorService.invokeAny(callables, l, timeUnit)
          }
        }
}
