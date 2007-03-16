
package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException, System}

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

/**
 * FJTaskScheduler2
 *
 * @version 0.9.5
 * @author Philipp Haller
 */
class FJTaskScheduler2 extends Thread with IScheduler {

  val printStats = false
  //val printStats = true

  val coreProp = System.getProperty("actors.corePoolSize")
  val maxProp = System.getProperty("actors.maxPoolSize")

  val initCoreSize =
    if (null ne coreProp) Integer.parseInt(coreProp)
    else 4

  val maxSize =
    if (null ne maxProp) Integer.parseInt(maxProp)
    else 256

  private var coreSize = initCoreSize

  private val executor =
    new FJTaskRunnerGroup(coreSize)

  private var terminating = false
  private var suspending = false

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
  def terminated(a: Actor) {}

  private val TICK_FREQ = 50
  private val CHECK_FREQ = 100

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

          if (!suspending) {
            // check if we need more threads
            if (Platform.currentTime - lastActivity >= TICK_FREQ
                && coreSize < maxSize
                && executor.checkPoolSize()) {
                  // do nothing
                }
            else {
              if (pendingReactions <= 0) {
                // if all worker threads idle terminate
                if (executor.getActiveCount() == 0) {
                  // Note that we don't have to shutdown
                  // the FJTaskRunnerGroup since there is
                  // no separate thread associated with it,
                  // and FJTaskRunner threads have daemon status.

                  // terminate timer thread
                  TimerThread.t.interrupt()
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
        if (printStats) executor.stats()
    }
  }

  /**
   *  @param item the task to be executed.
   */
  def execute(task: Reaction) {
    executor.execute(task)
  }

  def execute(task: FJTask) {
    executor.execute(task)
  }

  def start(task: Reaction) {
    this.synchronized {
      pendingReactions = pendingReactions + 1
    }
    executor.execute(task)
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
    // terminate timer thread
    TimerThread.t.interrupt()
  }

  def snapshot(): LinkedQueue = synchronized {
    suspending = true
    executor.snapshot()
    // grab tasks from executor
    executor.entryQueue
  }


}
