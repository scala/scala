/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException, System, Runtime}
import java.lang.Thread.State

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

import scheduler.{ThreadPoolConfig, QuitException}

/**
 * FJTaskScheduler2
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
class FJTaskScheduler2(val initCoreSize: Int, val maxSize: Int, daemon: Boolean) extends Thread with IScheduler with ActorGC {
  setDaemon(daemon)

  /** Default constructor creates a non-daemon thread. */
  def this() =
    this(ThreadPoolConfig.corePoolSize, ThreadPoolConfig.maxPoolSize, false)

  def this(daemon: Boolean) =
    this(ThreadPoolConfig.corePoolSize, ThreadPoolConfig.maxPoolSize, daemon)

  var printStats = false

  private var coreSize = initCoreSize

  private val executor =
    new FJTaskRunnerGroup(coreSize)

  private var terminating = false
  private var suspending = false

  private var submittedTasks = 0

  private val CHECK_FREQ = 100

  private def allWorkersBlocked: Boolean =
    executor.threads.forall(t => {
      val s = t.getState()
      s == State.BLOCKED || s == State.WAITING || s == State.TIMED_WAITING
    })

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

            gc()

            // check if we need more threads
            if (coreSize < maxSize
                && allWorkersBlocked
                && executor.checkPoolSize()) {
                  //Debug.info(this+": increasing thread pool size")
                  coreSize += 1
                }
            else {
              if (allTerminated) {
                // if all worker threads idle terminate
                if (executor.getActiveCount() == 0) {
                  Debug.info(this+": initiating shutdown...")

                  // Note that we don't have to shutdown
                  // the FJTaskRunnerGroup since there is
                  // no separate thread associated with it,
                  // and FJTaskRunner threads have daemon status.
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
   *  @param  task the task to be executed
   */
  def execute(task: Runnable): Unit =
    executor execute task

  def execute(fun: => Unit): Unit =
    executor.execute(new Runnable {
      def run() { fun }
    })

  /** Shuts down all idle worker threads.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
  }

  def snapshot(): LinkedQueue = {
    suspending = true
    executor.snapshot()
  }

  def isActive = !terminating && !suspending

  def managedBlock(blocker: scala.concurrent.ManagedBlocker) {
    blocker.block()
  }
}
