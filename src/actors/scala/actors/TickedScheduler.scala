
package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException}

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

/**
 * <p>This scheduler uses a thread pool to execute tasks that are generated
 * by the execution of actors. Unlike <code>ThreadPoolScheduler</code>, this
 * scheduler is available on all Java versions >= 1.4.</p>
 *
 * @version 0.9.5
 * @author Philipp Haller
 */
class TickedScheduler extends Thread with IScheduler {
  private val tasks = new Queue[Reaction]

  // Worker threads
  private val workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]
  private val idle = new Queue[WorkerThread]
  private val ticks = new HashMap[WorkerThread, long]

  private var terminating = false

  private var lastActivity = Platform.currentTime

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
    execute(task)
  }

  def terminated(a: Actor) {}

  private var TICK_FREQ = 5
  private var CHECK_FREQ = 50

  for (val i <- List.range(0, 2)) {
    val worker = new WorkerThread(this)
    workers += worker
    worker.start()
  }

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
            if (pendingReactions == 0) {
              // if all worker threads idle terminate
              if (workers.length == idle.length) {
                val idleThreads = idle.elements
                while (idleThreads.hasNext) {
                  val worker = idleThreads.next
                  worker.running = false
                  worker.interrupt()
                }
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
    if (!terminating) {
      if (idle.length > 0) {
        val wt = idle.dequeue
        wt.execute(item)
      }
      else
        tasks += item
    }
  }

  def execute(task: FJTask) { }

  def snapshot(): LinkedQueue = null

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
  def shutdown(): unit = synchronized {
    terminating = true
    val idleThreads = idle.elements
    while (idleThreads.hasNext) {
      val worker = idleThreads.next
      worker.running = false
      worker.interrupt()
    }
    // terminate timer thread
    TimerThread.t.interrupt()
  }
}
