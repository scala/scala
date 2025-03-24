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

package scala.tools.nsc.profile

import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation._
import scala.tools.nsc.{Global, Phase}

sealed trait ThreadPoolFactory {
  def newUnboundedQueueFixedThreadPool(
      nThreads: Int,
      shortId: String,
      priority: Int = Thread.NORM_PRIORITY): ThreadPoolExecutor

  def newBoundedQueueFixedThreadPool(
      nThreads: Int,
      maxQueueSize: Int,
      rejectHandler: RejectedExecutionHandler,
      shortId: String,
      priority: Int = Thread.NORM_PRIORITY): ThreadPoolExecutor
}

object ThreadPoolFactory {
  def apply(global: Global, phase: Phase): ThreadPoolFactory = global.currentRun.profiler match {
    case NoOpProfiler => new BasicThreadPoolFactory(phase)
    case r: RealProfiler => new ProfilingThreadPoolFactory(phase, r)
  }

  private abstract class BaseThreadPoolFactory(phase: Phase) extends ThreadPoolFactory {
    val baseGroup = new ThreadGroup(s"scalac-${phase.name}")

    private def childGroup(name: String) = new ThreadGroup(baseGroup, name)

    // Invoked when a new `Worker` is created, see `CommonThreadFactory.newThread`
    protected def wrapWorker(worker: Runnable, @unused shortId: String): Runnable = worker

    protected final class CommonThreadFactory(
        shortId: String,
        daemon: Boolean = true,
        priority: Int) extends ThreadFactory {
      private val group: ThreadGroup = childGroup(shortId)
      private val threadNumber: AtomicInteger = new AtomicInteger(1)
      private val namePrefix = s"${baseGroup.getName}-$shortId-"

      // Invoked by the `ThreadPoolExecutor` when creating a new worker thread. The argument
      // runnable is the `Worker` (which extends `Runnable`). Its `run` method gets tasks from
      // the thread pool and executes them (on the thread created here).
      override def newThread(worker: Runnable): Thread = {
        val wrapped = wrapWorker(worker, shortId)
        val t: Thread = new Thread(group, wrapped, namePrefix + threadNumber.getAndIncrement, 0)
        if (t.isDaemon != daemon) t.setDaemon(daemon)
        if (t.getPriority != priority) t.setPriority(priority)
        t
      }
    }
  }

  private final class BasicThreadPoolFactory(phase: Phase) extends BaseThreadPoolFactory(phase) {
    override def newUnboundedQueueFixedThreadPool(nThreads: Int, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, priority = priority)
      //like Executors.newFixedThreadPool
      new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory)
    }

    override def newBoundedQueueFixedThreadPool(nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, priority = priority)
      //like Executors.newFixedThreadPool
      new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize), threadFactory, rejectHandler)
    }
  }

  private class ProfilingThreadPoolFactory(phase: Phase, profiler: RealProfiler) extends BaseThreadPoolFactory(phase) {
    override def newUnboundedQueueFixedThreadPool(nThreads: Int, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, priority = priority)
      //like Executors.newFixedThreadPool
      new SinglePhaseInstrumentedThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory, new AbortPolicy)
    }

    override def newBoundedQueueFixedThreadPool(nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, priority = priority)
      //like Executors.newFixedThreadPool
      new SinglePhaseInstrumentedThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize), threadFactory, rejectHandler)
    }

    override protected def wrapWorker(worker: Runnable, shortId: String): Runnable = () => {
      val data = new ThreadProfileData
      localData.set(data)

      val profileStart = RealProfiler.snapThread(0)
      try worker.run finally {
        val snap = RealProfiler.snapThread(data.idleNs)
        val threadRange = ProfileRange(profileStart, snap, phase, shortId, data.taskCount, Thread.currentThread())
        profiler.completeBackground(threadRange)
      }
    }

    /**
      * data for thread run. Not threadsafe, only written from a single thread
      */
    final class ThreadProfileData {
      var firstStartNs = 0L
      var taskCount = 0

      var idleNs = 0L
      var runningNs = 0L

      var lastStartNs = 0L
      var lastEndNs = 0L
    }

    val localData = new ThreadLocal[ThreadProfileData]

    private class SinglePhaseInstrumentedThreadPoolExecutor(
        corePoolSize: Int, maximumPoolSize: Int, keepAliveTime: Long, unit: TimeUnit,
        workQueue: BlockingQueue[Runnable], threadFactory: ThreadFactory, handler: RejectedExecutionHandler)
      extends ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler) {

      override def beforeExecute(t: Thread, r: Runnable): Unit = {
        val data = localData.get
        data.taskCount += 1
        val now = System.nanoTime()

        if (data.firstStartNs == 0) data.firstStartNs = now
        else data.idleNs += now - data.lastEndNs

        data.lastStartNs = now

        super.beforeExecute(t, r)
      }

      override def afterExecute(r: Runnable, t: Throwable): Unit = {
        val now = System.nanoTime()
        val data = localData.get

        data.lastEndNs = now
        data.runningNs += now - data.lastStartNs

        super.afterExecute(r, t)
      }
    }
  }
}
