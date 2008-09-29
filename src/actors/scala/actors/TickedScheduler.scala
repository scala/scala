/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.lang.{Thread, InterruptedException}

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue}
import scala.compat.Platform

/**
 * <p>This scheduler uses a thread pool to execute tasks that are generated
 * by the execution of actors.</p>
 *
 * Use class <code>FJTaskScheduler2</code> instead.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
@deprecated
class TickedScheduler extends Thread with WorkerThreadScheduler {
  // as long as this thread runs, JVM should not exit
  setDaemon(false)

  private val tasks = new Queue[Runnable]

  // Worker threads
  private val workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]
  private val idle = new Queue[WorkerThread]
  private val ticks = new HashMap[WorkerThread, Long]

  private var terminating = false

  private var lastActivity = Platform.currentTime

  def printActorDump {}

  private var TICK_FREQ = 5
  private var CHECK_FREQ = 50

  for (i <- List.range(0, 2)) {
    val worker = new WorkerThread(this)
    workers += worker
    worker.start()
  }

  def onLockup(handler: () => Unit) {
    lockupHandler = handler
  }

  def onLockup(millis: Int)(handler: () => Unit) {
    //LOCKUP_CHECK_FREQ = millis / CHECK_FREQ
    lockupHandler = handler
  }

  private var lockupHandler: () => Unit = null

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

          ActorGC.gc()

          if (tasks.length > 0) {
            // check if we need more threads
            if (Platform.currentTime - lastActivity >= TICK_FREQ) {
              val newWorker = new WorkerThread(this)
              workers += newWorker

              // dequeue item to be processed
              val item = tasks.dequeue

              newWorker.execute(item)
              newWorker.start()
            }
          } // tasks.length > 0
          else {
            if (ActorGC.allTerminated) {
              // if all worker threads idle terminate
              if (workers.length == idle.length) {
                Debug.info(this+": initiating shutdown...")

                val idleThreads = idle.elements
                while (idleThreads.hasNext) {
                  val worker = idleThreads.next
                  worker.running = false
                  worker.interrupt()
                }
                // terminate timer thread
                Actor.timer.cancel()
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
  def execute(item: Runnable): Unit = synchronized {
    if (!terminating) {
      if (idle.length > 0) {
        val wt = idle.dequeue
        wt.execute(item)
      }
      else
        tasks += item
    }
  }

  def execute(fun: => Unit): Unit =
    execute(new Runnable {
      def run() { fun }
    })

  /**
   *  @param worker the worker thread executing tasks
   *  @return       the executed task
   */
  def getTask(worker: WorkerThread) = synchronized {
    if (terminating)
      QUIT_TASK
    if (tasks.length > 0) {
      val item = tasks.dequeue
      item
    }
    else {
      idle += worker
      null
    }
  }

  /**
   *  @param a the actor
   */
  def tick(a: Actor) {
    lastActivity = Platform.currentTime
  }

  /** Shuts down all idle worker threads.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
    val idleThreads = idle.elements
    while (idleThreads.hasNext) {
      val worker = idleThreads.next
      worker.running = false
      worker.interrupt()
    }
    // terminate timer thread
    Actor.timer.cancel()
  }
}
