
package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException}

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

import java.util.concurrent.{ThreadPoolExecutor,
                             LinkedBlockingQueue,
                             TimeUnit,
                             RejectedExecutionHandler}

/**
 * This handler is called whenever the thread pool of its
 * associated <code>ThreadPoolScheduler</code> is unable
 * to serve a request to execute some task.
 *
 * This handler executes rejected tasks on the thread of
 * the scheduler.
 *
 * @version 0.9.5
 * @author Philipp Haller
 */
private class TaskRejectedHandler(sched: ThreadPoolScheduler) extends RejectedExecutionHandler {
  def rejectedExecution(r: Runnable, executor: ThreadPoolExecutor) {
    sched.pendReaction
    r.run()
    sched.unPendReaction
  }
}

/**
 * <p>This scheduler uses a thread pool to execute tasks that are generated
 * by the execution of actors. This scheduler is only available on Java >= 1.5
 * since it uses <code>java.util.concurrent.ThreadPoolExecutor</code>.</p>
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
class ThreadPoolScheduler extends Thread with IScheduler {

  var initCoreSize = 4
  var maxSize = 16

  val prop = java.lang.System.getProperty("actors.corePoolSize")
  if (null ne prop) {
    initCoreSize =
      Integer.parseInt(java.lang.System.getProperty("actors.corePoolSize"))
    maxSize =
      Integer.parseInt(java.lang.System.getProperty("actors.maxPoolSize"))
  }

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
                           new LinkedBlockingQueue,
                           new TaskRejectedHandler(this))

  private var coreSize = initCoreSize

  private var terminating = false

  private var lastActivity = Platform.currentTime

  private var submittedTasks = 0

  private var pendingReactions = 0
  def pendReaction: unit = synchronized {
    pendingReactions = pendingReactions + 1
  }
  def unPendReaction: unit = synchronized {
    pendingReactions = pendingReactions - 1
  }

  def printActorDump {}

  def start(task: Reaction): unit = synchronized {
    pendingReactions = pendingReactions + 1
    submittedTasks = submittedTasks + 1
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
                executor.shutdown()
                // terminate timer thread
                TimerThread.t.interrupt()
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
    submittedTasks = submittedTasks + 1
    executor.execute(item)
  }

  def execute(task: FJTask) { }

  def snapshot(): LinkedQueue = null

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
    // terminate timer thread
    TimerThread.t.interrupt()
  }
}
