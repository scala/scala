package scala.tools.nsc.profile

import java.util.Collections
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.tools.nsc.{Global, Phase}

sealed trait AsyncHelper {

  def newUnboundedQueueFixedThreadPool
  (nThreads: Int,
   shortId: String, priority : Int = Thread.NORM_PRIORITY) : ThreadPoolExecutor
  def newBoundedQueueFixedThreadPool
  (nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler,
   shortId: String, priority : Int = Thread.NORM_PRIORITY) : ThreadPoolExecutor

}

object AsyncHelper {
  def apply(global: Global, phase: Phase): AsyncHelper = global.currentRun.profiler match {
    case NoOpProfiler => new BasicAsyncHelper(global, phase)
    case r: RealProfiler => new ProfilingAsyncHelper(global, phase, r)
  }

  private abstract class BaseAsyncHelper(global: Global, phase: Phase) extends AsyncHelper {
    val baseGroup = new ThreadGroup(s"scalac-${phase.name}")
    private def childGroup(name: String) = new ThreadGroup(baseGroup, name)

    protected def wrapRunnable(r: Runnable, shortId:String): Runnable

    protected class CommonThreadFactory(shortId: String,
                                        daemon: Boolean = true,
                                        priority: Int) extends ThreadFactory {
      private val group: ThreadGroup = childGroup(shortId)
      private val threadNumber: AtomicInteger = new AtomicInteger(1)
      private val namePrefix = s"${baseGroup.getName}-$shortId-"

      override def newThread(r: Runnable): Thread = {
        val wrapped = wrapRunnable(r, shortId)
        val t: Thread = new Thread(group, wrapped, namePrefix + threadNumber.getAndIncrement, 0)
        if (t.isDaemon != daemon) t.setDaemon(daemon)
        if (t.getPriority != priority) t.setPriority(priority)
        t
      }
    }
  }

  private final class BasicAsyncHelper(global: Global, phase: Phase) extends BaseAsyncHelper(global, phase) {

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

    override protected def wrapRunnable(r: Runnable, shortId:String): Runnable = r
  }

  private class ProfilingAsyncHelper(global: Global, phase: Phase, private val profiler: RealProfiler) extends BaseAsyncHelper(global, phase) {

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

    override protected def wrapRunnable(r: Runnable, shortId:String): Runnable = () => {
      val data = new ThreadProfileData
      localData.set(data)

      val profileStart = profiler.snapThread(0)
      try r.run finally {
        val snap = profiler.snapThread(data.idleNs)
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

    private class SinglePhaseInstrumentedThreadPoolExecutor
        (   corePoolSize: Int, maximumPoolSize: Int, keepAliveTime: Long, unit: TimeUnit,
            workQueue: BlockingQueue[Runnable], threadFactory: ThreadFactory, handler: RejectedExecutionHandler
    ) extends ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler) {

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