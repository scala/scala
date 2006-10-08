/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.actors

import scala.collection.mutable.{ArrayBuffer, Buffer, Queue}

/**
 * The <code>Scheduler</code> object is used by
 * <code>Reactor</code> to execute tasks of an execution of a
 * reactor.
 *
 * @author Philipp Haller
 */
object Scheduler {
  private var sched: IScheduler =
    new SpareWorkerScheduler

  def impl = sched
  def impl_= (scheduler: IScheduler) = {
    sched = scheduler
  }

  def execute(task: Reaction) = synchronized {
    sched.execute(task)
  }

  def tick(a: Reactor) = sched.tick(a)

  def shutdown(): Unit = sched.shutdown()
}

/**
 * This abstract class provides a common interface for all
 * schedulers used to execute reactors.
 *
 * @author Philipp Haller
 */
abstract class IScheduler {
  def execute(task: Reaction): Unit
  def getTask(worker: WorkerThread): Runnable
  def tick(a: Reactor): Unit

  def shutdown(): Unit

  val QUIT_TASK = new Reaction() {
    def actor: Reactor = null
    def run(): Unit = {}
    override def toString() = "QUIT_TASK"
  }
}

/**
 * This scheduler executes the tasks of a reactor on a single
 * thread (the current thread).
 *
 * @author Philipp Haller
 */
class SingleThreadedScheduler extends IScheduler {
  def execute(task: Reaction): Unit = {
    // execute task immediately on same thread
    task.run()
  }

  def getTask(worker: WorkerThread): Runnable = { null }

  def tick(a: Reactor): Unit = {}

  def shutdown(): Unit = {}
}

/**
 * This scheduler creates additional threads whenever there is no
 * idle thread available.
 *
 * @author Philipp Haller
 */
class SpareWorkerScheduler extends IScheduler {
  private val tasks = new Queue[Reaction]
  private val idle = new Queue[WorkerThread]
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]

  private var maxWorkers = 2

  def init() = {
    for (val i <- 0 until 2) {
      val worker = new WorkerThread(this)
      workers += worker
      worker.start()
    }
  }
  init()

  def execute(task: Reaction): Unit = synchronized {
    if (!terminating) {
      if (idle.length == 0) {
        tasks += task
        // create new worker
        maxWorkers = maxWorkers + 1
        val newWorker = new WorkerThread(this)
        workers += newWorker
        newWorker.start()
      }
      else {
        idle.dequeue.execute(task)
      }
    }
  }

  def getTask(worker: WorkerThread) = synchronized {
    if (tasks.length > 0) tasks.dequeue
    else {
      idle += worker
      null
    }
  }

  def tick(a: Reactor): Unit = {}

  private var terminating = false

  def shutdown(): Unit = synchronized {
    terminating = true
    val numNonIdle = workers.length - idle.length
    for (val i <- 0 until numNonIdle)
      tasks += QUIT_TASK
    val idleThreads = idle.elements
    while (idleThreads.hasNext) {
      val worker = idleThreads.next
      worker.interrupt()
      worker.join()
    }
  }
}

/**
 * This class is used by schedulers to execute reactor tasks on
 * multiple threads.
 *
 * @author Philipp Haller
 */
class WorkerThread(sched: IScheduler) extends Thread {
  private var task: Runnable = null
  private var running = true

  def execute(r: Runnable) = synchronized {
    task = r
    notify()
  }

  override def run(): Unit = synchronized {
    try {
      while (running) {
        if (task != null) task.run()
        task = sched.getTask(this)
        if (task == sched.QUIT_TASK) {
          running = false
        } else if (task == null) wait()
      }
    } catch {
      case consumed: InterruptedException =>
        // allow thread to quit
    }
  }
}
