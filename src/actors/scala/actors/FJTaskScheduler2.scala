/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.lang.{Runnable, Thread, InterruptedException, System, Runtime}
import scala.actors.threadpool.{ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue}

/**
 * FJTaskScheduler2
 *
 * @version 0.9.19
 * @author Philipp Haller
 */
class FJTaskScheduler2 extends Thread with IScheduler {
  // as long as this thread runs, JVM should not exit
  setDaemon(false)

  @deprecated var printStats = false

  @deprecated val rt = Runtime.getRuntime()
  private val numCores = rt.availableProcessors()
  @deprecated val minNumThreads = 4

  @deprecated val coreProp = try {
    System.getProperty("actors.corePoolSize")
  } catch {
    case ace: java.security.AccessControlException =>
      null
  }
  @deprecated val maxProp =
    try {
      System.getProperty("actors.maxPoolSize")
    } catch {
      case ace: java.security.AccessControlException =>
        null
    }
  @deprecated val timeFreqProp =
    try {
      System.getProperty("actors.timeFreq")
    } catch {
      case ace: java.security.AccessControlException =>
        null
    }

  val initCoreSize =
    if (null ne coreProp) Integer.parseInt(coreProp)
    else {
      if (2 * numCores > minNumThreads)
        2 * numCores
      else
        minNumThreads
    }

  val maxSize =
    if (null ne maxProp) Integer.parseInt(maxProp)
    else 256

  @deprecated val timeFreq =
    if (null ne timeFreqProp) Integer.parseInt(timeFreqProp)
    else 10

  private var coreSize = initCoreSize

  Debug.info(this+": corePoolSize = "+coreSize+", maxPoolSize = "+maxSize)

  private val executor = {
    val workQueue = new LinkedBlockingQueue[Runnable]

    new ThreadPoolExecutor(coreSize,
                           maxSize,
                           60000L,
                           TimeUnit.MILLISECONDS,
                           workQueue,
                           new ThreadPoolExecutor.CallerRunsPolicy)
  }

  @volatile private var terminating = false
  private var suspending = false

  private var submittedTasks = 0

  @deprecated def printActorDump {}

  private val TICK_FREQ = 50
  private val CHECK_FREQ = timeFreq

  @deprecated def onLockup(handler: () => Unit) =
    lockupHandler = handler

  @deprecated def onLockup(millis: Int)(handler: () => Unit) = {
    //LOCKUP_CHECK_FREQ = millis / CHECK_FREQ
    lockupHandler = handler
  }

  private var lockupHandler: () => Unit = null

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
      while (!terminating) {
        this.synchronized {
          try {
            wait(CHECK_FREQ)
          } catch {
            case _: InterruptedException =>
              if (terminating) throw new QuitException
          }

          if (!suspending) {
            // check if we need more worker threads
            val activeBlocked = numWorkersBlocked
            if (coreSize - activeBlocked < numCores && coreSize < maxSize) {
              coreSize = numCores + activeBlocked
              Debug.info(this+": increasing thread pool size to "+coreSize)
              executor.setCorePoolSize(coreSize)
            } else {
              if (ActorGC.allTerminated) {
                // if all worker threads idle terminate
                if (executor.getActiveCount() == 0) {
                  Debug.info(this+": initiating shutdown...")
                  Debug.info(this+": corePoolSize = "+coreSize+", maxPoolSize = "+maxSize)

                  terminating = true
                  throw new QuitException
                }
              }
            }
          }
        } // sync

      } // while (!terminating)
    } catch {
      case _: QuitException =>
        // allow thread to exit
        //if (printStats) executor.stats()
        executor.shutdown()
    }
  }

  /**
   *  @param  task the task to be executed
   */
  def execute(task: Runnable): Unit =
    executor execute task

  def execute(fun: => Unit): Unit =
    executor.execute(new Runnable {
      def run() { fun }
    })

  /**
   *  @param  a the actor
   */
  def tick(a: Actor) = {}

  /** Shuts down all idle worker threads.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
  }

  @deprecated def snapshot(): LinkedQueue = {
    suspending = true
    val tasks: java.util.List[_] = executor.shutdownNow()
    val linkedQ = new LinkedQueue
    val iter = tasks.iterator()
    while (iter.hasNext()) {
      linkedQ put iter.next()
    }
    linkedQ
  }

  private[actors] override def isActive =
    !terminating
}
