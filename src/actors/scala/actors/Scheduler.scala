/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue}

/**
 * The <code>Scheduler</code> object is used by
 * <code>Actor</code> to execute tasks of an execution of a
 * reactor.
 *
 * @version Beta2
 * @author Philipp Haller
 */
object Scheduler {
  private var sched: IScheduler =
    //new SpareWorkerScheduler
    new TickedScheduler

  def impl = sched
  def impl_= (scheduler: IScheduler) = {
    sched = scheduler
  }

  def execute(task: Reaction) = synchronized {
    sched.execute(task)
  }

  def tick(a: Actor) = sched.tick(a)

  def shutdown(): Unit = sched.shutdown()
}

/**
 * This abstract class provides a common interface for all
 * schedulers used to execute reactors.
 *
 * @version Beta2
 * @author Philipp Haller
 */
abstract class IScheduler {
  def execute(task: Reaction): Unit
  def getTask(worker: WorkerThread): Runnable
  def tick(a: Actor): Unit

  def shutdown(): Unit

  val QUIT_TASK = new Reaction() {
    def actor: Actor = null
    def run(): Unit = {}
    override def toString() = "QUIT_TASK"
  }
}

/**
 * This scheduler executes the tasks of a reactor on a single
 * thread (the current thread).
 *
 * @version Beta2
 * @author Philipp Haller
 */
class SingleThreadedScheduler extends IScheduler {
  def execute(task: Reaction): Unit = {
    // execute task immediately on same thread
    task.run()
  }

  def getTask(worker: WorkerThread): Runnable = { null }

  def tick(a: Actor): Unit = {}

  def shutdown(): Unit = {}
}

/**
 * This scheduler creates additional threads whenever there is no
 * idle thread available.
 *
 * @version Beta2
 * @author Philipp Haller
 */
class SpareWorkerScheduler extends IScheduler {
  private val tasks = new Queue[Reaction]
  private val idle = new Queue[WorkerThread]
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]

  private var terminating = false

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
        val newWorker = new WorkerThread(this)
        workers += newWorker
        newWorker.start()
      }
      else {
        val worker = idle.dequeue
        worker.execute(task)
      }
    }
  }

  def getTask(worker: WorkerThread) = synchronized {
    if (terminating)
      QUIT_TASK
    else {
      if (tasks.length > 0) tasks.dequeue
      else {
        idle += worker
        null
      }
    }
  }

  def tick(a: Actor): Unit = {}

  def shutdown(): Unit = synchronized {
    terminating = true

    val idleThreads = idle.elements
    while (idleThreads.hasNext) {
      val worker = idleThreads.next
      worker.running = false
      worker.interrupt()
      // caused deadlock (tries to acquire lock of worker)
      //worker.join()
    }
  }
}

/**
 * The class <code>TickedScheduler</code> ...
 *
 * @author Philipp Haller
 */
class TickedScheduler extends IScheduler {
  private val tasks = new Queue[Reaction]
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]

  private val idle = new Queue[WorkerThread]
  private val ticks = new HashMap[WorkerThread, long]
  private val executing = new HashMap[Actor, WorkerThread]

  private var terminating = false

  var TICKFREQ = 50

  def init() = {
    for (val i <- List.range(0, 2)) {
      val worker = new WorkerThread(this)
      workers += worker
      worker.start()
    }
  }
  init()

  def execute(item: Reaction): unit = synchronized {
    if (!terminating)
      if (idle.length > 0) {
        val wt = idle.dequeue
        executing.update(item.actor, wt)
        wt.execute(item)
      }
      else {
        /*
         only create new worker thread when all are blocked
         according to heuristic

         we check time stamps of latest send/receive ops of ALL
         workers

         we stop if there is one which is not blocked
        */

        val iter = workers.elements
        var foundBusy = false
        while (iter.hasNext && !foundBusy) {
          val wt = iter.next
          ticks.get(wt) match {
            case None => foundBusy = true // assume not blocked
            case Some(ts) => {
              val currTime = System.currentTimeMillis
              if (currTime - ts < TICKFREQ)
                foundBusy = true
            }
          }
        }

        if (!foundBusy) {
          val newWorker = new WorkerThread(this)
          workers += newWorker
          executing.update(item.actor, newWorker)
          newWorker.execute(item)
          newWorker.start()
        }
        else {
          // wait assuming busy thread will be finished soon
          // and ask for more work
          tasks += item
        }
      }
  }

  def getTask(worker: WorkerThread) = synchronized {
    if (terminating)
      QUIT_TASK
    if (tasks.length > 0) {
      val item = tasks.dequeue
      executing.update(item.actor, worker)
      item
    }
    else {
      idle += worker
      null
    }
  }

  var ticksCnt = 0

  def tick(a: Actor): unit = synchronized {
    ticksCnt = ticksCnt + 1
    executing.get(a) match {
      case None => // thread outside of scheduler; error("No worker thread associated with actor " + a)
      case Some(wt) =>
        ticks.update(wt, System.currentTimeMillis)
    }
  }

  def shutdown(): Unit = synchronized {
    terminating = true

    val idleThreads = idle.elements
    while (idleThreads.hasNext) {
      val worker = idleThreads.next
      worker.running = false
      worker.interrupt()
      // caused deadlock (tries to acquire lock of worker)
      //worker.join()
    }
  }
}


class QuitException extends Throwable {
  /*
   For efficiency reasons we do not fill in
   the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = {
    this
  }
}


/**
 * The class <code>WorkerThread</code> is used by schedulers to execute
 * actor tasks on multiple threads.
 *
 * !!ACHTUNG: If you change this, make sure you understand the following
 * proof of deadlock-freedom!!
 *
 * We proof that there is no deadlock between the scheduler and
 * any worker thread possible. For this, note that the scheduler
 * only acquires the lock of a worker thread by calling
 * `execute'.  This method is only called when the worker thread
 * is in the idle queue of the scheduler. On the other hand, a
 * worker thread only acquires the lock of the scheduler when it
 * calls `getTask'. At the only callsite of `getTask', the
 * worker thread holds its own lock.
 *
 * Thus, deadlock can only occur when a worker thread calls
 * `getTask' while it is in the idle queue of the scheduler,
 * because then the scheduler might call (at any time!) `execute'
 * which tries to acquire the lock of the worker thread. In such
 * a situation the worker thread would be waiting to acquire the
 * lock of the scheduler and vice versa.
 *
 * Therefore, to prove deadlock-freedom, it suffices to ensure
 * that a worker thread will never call `getTask' when it is in
 * the idle queue of the scheduler.
 *
 * A worker thread enters the idle queue of the scheduler when
 * `getTask' returns <code>null</code>. Then it will also stay
 * in the while-loop W (<code>while (task == null)</code>) until
 * `task' becomes non-null. The only way this can happen is
 * through a call of `execute' by the scheduler. Before every
 * call of `execute' the worker thread is removed from the idle
 * queue of the scheduler. Only then--after executing its task--
 * the worker thread may call `getTask'. However, the scheduler
 * is unable to call `execute' as the worker thread is not in
 * the idle queue any more. In fact, the scheduler made sure
 * that this is the case even _before_ calling `execute' and
 * thus releasing the worker thread from the while-loop W. Thus,
 * the property holds for every possible interleaving of thread
 * execution. QED
 *
 *
 * @version Beta2
 * @author Philipp Haller
 */
class WorkerThread(sched: IScheduler) extends Thread {
  private var task: Runnable = null
  private[actors] var running = true

  def execute(r: Runnable) = synchronized {
    task = r
    notify()
  }

  override def run(): Unit = {
    try {
      while (running) {
        if (task != null) {
          try {
            task.run()
          } catch {
            case consumed: InterruptedException => {
              if (!running) throw new QuitException
            }
          }
        }
        this.synchronized {
          task = sched.getTask(this)

          while (task == null) {
            try {
              wait()
            } catch {
              case consumed: InterruptedException => {
                if (!running) throw new QuitException
              }
            }
          }

          if (task == sched.QUIT_TASK) {
            running = false
          }
        }
      }
    } catch {
      case consumed: QuitException =>
        // allow thread to quit
    }
  }
}
