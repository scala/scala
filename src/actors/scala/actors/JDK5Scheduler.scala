
package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException}

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

import java.util.concurrent.{ThreadPoolExecutor,
                             LinkedBlockingQueue,
                             TimeUnit}

/**
 *
 * @version 0.9.2
 * @author Philipp Haller
 */
class JDK5Scheduler(initCoreSize: int, maxSize: int) extends Thread with IScheduler {

  //Debug.info("using JDK5Scheduler")

  /* Note:
   * When using an unbounded queue such as a
   * LinkedBlockingQueue, the executor never creates
   * more than coreSize threads. Therefore, we pass
   * coreSize also as the maxPoolSize parameter.
   *
   * Our maxSize controls how much we dynamically increase
   * the pool's coreSize.
   */
  private val executor =
    new ThreadPoolExecutor(initCoreSize,
                           initCoreSize,
                           5000,
                           TimeUnit.NANOSECONDS,
                           new LinkedBlockingQueue)

  private var coreSize = initCoreSize

  private var terminating = false

  private var lastActivity = Platform.currentTime

  private var pendingReactions = 0
  def pendReaction: unit = synchronized {
    //Debug.info("pend reaction")
    pendingReactions = pendingReactions + 1
  }
  def unPendReaction: unit = synchronized {
    //Debug.info("unpend reaction")
    pendingReactions = pendingReactions - 1
  }

  def printActorDump {}

  def start(task: Reaction): unit = synchronized {
    //Debug.info("Starting " + task.actor)
    execute(task)
  }

  def terminated(a: Actor) {}

  private var TICK_FREQ = 5
  private var CHECK_FREQ = 50

  def onLockup(handler: () => unit) =
    lockupHandler = handler

  def onLockup(millis: int)(handler: () => unit) = {
    //LOCKUP_CHECK_FREQ = millis / CHECK_FREQ
    lockupHandler = handler
  }

  private var lockupHandler: () => unit = null

  override def run(): unit = {
    try {
      while (!terminating) {
        this.synchronized {
          try {
            wait(CHECK_FREQ)
          } catch {
            case _: InterruptedException =>
              if (terminating) throw new QuitException
          }

          //Debug.info("tasks.length: "+executor.getQueue().size())
          //Debug.info("pendingReactions: "+pendingReactions)

          // check if we need more threads
          if (executor.getQueue().size() > 0
              && Platform.currentTime - lastActivity >= TICK_FREQ
              && coreSize < maxSize) {
            coreSize = coreSize + 1
            // increase corePoolSize of thread pool
            executor.setCorePoolSize(coreSize)
          }
          else {
            if (pendingReactions == 0) {
              // if all worker threads idle terminate
              if (executor.getActiveCount() == 0) {
                //Debug.info("all threads idle, terminating")
                executor.shutdown()
                // terminate timer thread
                TimerThread.t.interrupt()
                Console.println("threads used: "+coreSize)
                throw new QuitException
              }
            }
          }
        } // sync

      } // while (!terminating)
    } catch {
      case _: QuitException =>
        // allow thread to exit
    }
  }

  /**
   *  @param item the task to be executed.
   */
  def execute(item: Reaction): unit = synchronized {
    executor.execute(item)
  }

  /**
   *  @param worker the worker thread executing tasks
   *  @return       the executed task
   */
  def getTask(worker: WorkerThread) = null

  /**
   *  @param a the actor
   */
  def tick(a: Actor) {
    lastActivity = Platform.currentTime
  }

  /** Shuts down all idle worker threads.
   */
  def shutdown(): unit = synchronized {
    terminating = true

    executor.shutdown()
  }
}
