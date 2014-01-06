/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors.scheduler

import scala.actors.threadpool.{ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue,
                                ThreadFactory}
import scala.actors.{Debug, IScheduler}
import scala.concurrent.ManagedBlocker

/**
 * This scheduler class uses a `ThreadPoolExecutor` to execute `Actor`s.
 *
 * The scheduler attempts to shut down itself and the underlying
 * `ThreadPoolExecutor` only if `terminate` is set to true. Otherwise,
 * the scheduler must be shut down explicitly.
 *
 * @author Philipp Haller
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
class ResizableThreadPoolScheduler(protected val terminate: Boolean,
                                   protected val daemon: Boolean)
  extends Thread with IScheduler with TerminationMonitor {

  setDaemon(daemon)

  // guarded by this
  private var terminating = false
  // guarded by this
  private var suspending = false

  // this has to be a java.util.Collection, since this is what
  // the ForkJoinPool returns.
  @volatile
  private var drainedTasks: java.util.List[_] = null

  // guarded by this
  private var coreSize = ThreadPoolConfig.corePoolSize
  private val maxSize = ThreadPoolConfig.maxPoolSize
  private val numCores = Runtime.getRuntime().availableProcessors()

  protected val CHECK_FREQ = 10

  private class DaemonThreadFactory extends ThreadFactory {
    def newThread(r: Runnable): Thread = {
      val t = new Thread(r)
      t.setDaemon(daemon)
      t
    }
  }
  private val threadFac = new DaemonThreadFactory

  private def makeNewPool(): ThreadPoolExecutor = {
    val workQueue = new LinkedBlockingQueue
    new ThreadPoolExecutor(coreSize,
                           maxSize,
                           60000L,
                           TimeUnit.MILLISECONDS,
                           workQueue,
                           threadFac,
                           new ThreadPoolExecutor.CallerRunsPolicy)
  }

  // guarded by this
  private var executor = makeNewPool()

  Debug.info(this+": corePoolSize = "+coreSize+", maxPoolSize = "+maxSize)

  def this(d: Boolean) {
    this(true, d)
  }

  def this() {
    this(false)
  }

  private def numWorkersBlocked = {
    executor.mainLock.lock()
    val iter = executor.workers.iterator()
    var numBlocked = 0
    while (iter.hasNext()) {
      val w = iter.next().asInstanceOf[ThreadPoolExecutor#Worker]
      if (w.tryLock()) {
        // worker is idle
        w.unlock()
      } else {
        val s = w.thread.getState()
        if (s == Thread.State.WAITING || s == Thread.State.TIMED_WAITING)
          numBlocked += 1
      }
    }
    executor.mainLock.unlock()
    numBlocked
  }

  override def run() {
    try {
      while (true) {
        this.synchronized {
          try {
            wait(CHECK_FREQ.toLong)
          } catch {
            case _: InterruptedException =>
          }

          if (terminating)
            throw new QuitControl

          if (!suspending) {
            gc()

            // check if we need more worker threads
            val activeBlocked = numWorkersBlocked
            if (coreSize - activeBlocked < numCores && coreSize < maxSize) {
              coreSize = numCores + activeBlocked
              executor.setCorePoolSize(coreSize)
            } else if (terminate && allActorsTerminated) {
              // if all worker threads idle terminate
              if (executor.getActiveCount() == 0) {
                Debug.info(this+": initiating shutdown...")
                Debug.info(this+": corePoolSize = "+coreSize+", maxPoolSize = "+maxSize)

                terminating = true
                throw new QuitControl
              }
            }
          } else {
            drainedTasks = executor.shutdownNow()
            Debug.info(this+": drained "+drainedTasks.size()+" tasks")
            terminating = true
            throw new QuitControl
          }
        } // sync
      }
    } catch {
      case _: QuitControl =>
        executor.shutdown()
        // allow thread to exit
    }
  }

  def execute(task: Runnable): Unit =
    executor execute task

  def execute(fun: => Unit): Unit =
    executor.execute(new Runnable {
      def run() { fun }
    })

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
  }

  def isActive = synchronized {
    !terminating && (executor ne null) && !executor.isShutdown()
  }

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

  /** Suspends the scheduler. All threads that were in use by the
   *  scheduler and its internal thread pool are terminated.
   */
  def snapshot() = synchronized {
    suspending = true
  }

  /** Resumes the execution of the scheduler if it was previously
   *  suspended using `snapshot`.
   */
  def restart() {
    synchronized {
      if (!suspending)
        sys.error("snapshot has not been invoked")
      else if (isActive)
        sys.error("scheduler is still active")
      else
        suspending = false

      executor = makeNewPool()
    }
    val iter = drainedTasks.iterator()
    while (iter.hasNext()) {
      executor.execute(iter.next().asInstanceOf[Runnable])
    }
    start()
  }

}
