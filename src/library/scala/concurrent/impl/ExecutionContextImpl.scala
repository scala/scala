/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.impl

import java.util.concurrent.{ ForkJoinPool, ForkJoinWorkerThread, Callable, Executor, ExecutorService, ThreadFactory, TimeUnit }
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

  final class DefaultThreadFactory(
    daemonic: Boolean,
    maxBlockers: Int,
    prefix: String,
    uncaught: Thread.UncaughtExceptionHandler) extends ThreadFactory with ForkJoinPool.ForkJoinWorkerThreadFactory {

    require(prefix ne null, "DefaultThreadFactory.prefix must be non null")
    require(maxBlockers >= 0, "DefaultThreadFactory.maxBlockers must be greater-or-equal-to 0")

    private final val currentNumberOfBlockers = new AtomicInteger(0)

    @tailrec private final def newBlocker(): Boolean = currentNumberOfBlockers.get() match {
      case `maxBlockers` | Int.`MaxValue` => false
      case other => currentNumberOfBlockers.compareAndSet(other, other + 1) || newBlocker()
    }

    @tailrec private final def freeBlocker(): Boolean = currentNumberOfBlockers.get() match {
      case 0 => false
      case other => currentNumberOfBlockers.compareAndSet(other, other - 1) || freeBlocker()
    }

    def wire[T <: Thread](thread: T): T = {
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler(uncaught)
      thread.setName(prefix + "-" + thread.getId())
      thread
    }

    def newThread(runnable: Runnable): Thread = wire(new Thread(runnable))

    def newThread(fjp: ForkJoinPool): ForkJoinWorkerThread =
      wire(new ForkJoinWorkerThread(fjp) with BlockContext {
        private[this] var isBlocked: Boolean = false // This is only ever read & written if this thread is the current thread
        final override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T =
          if ((Thread.currentThread eq this) && !isBlocked && newBlocker()) {
            try {
              isBlocked = true
              val b: ForkJoinPool.ManagedBlocker with (() => T) =
                new ForkJoinPool.ManagedBlocker with (() => T) {
                  private[this] var result: T = null.asInstanceOf[T]
                  private[this] var done: Boolean = false
                  final override def block(): Boolean = {
                    try {
                      if (!done)
                        result = thunk
                    } finally {
                      done = true
                    }

                    true
                  }

                  final override def isReleasable = done

                  final override def apply(): T = result
                }
              ForkJoinPool.managedBlock(b)
              b()
            } finally {
              isBlocked = false
              freeBlocker()
            }
          } else thunk // Unmanaged blocking
      })
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
                                                                      maxBlockers = maxExtraThreads,
                                                                      prefix = "scala-execution-context-global",
                                                                      uncaught = uncaughtExceptionHandler)

    new ForkJoinPool(desiredParallelism, threadFactory, uncaughtExceptionHandler, true)
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


