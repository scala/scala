/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException}
import java.util.logging.{Logger, FileHandler, Level}

import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack}

/**
 * The <code>Scheduler</code> object is used by
 * <code>Actor</code> to execute tasks of an execution of a
 * reactor.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
object Scheduler {
  private var sched: IScheduler =
    {
      val logger = Logger.getLogger("Scheduler")
      logger.addHandler(new FileHandler("sched.log"))
      logger.setLevel(Level.FINE)
      val s = new TickedScheduler
      s.start()
      s
    }

  def impl = sched
  def impl_= (scheduler: IScheduler) = {
    sched = scheduler
  }

  def execute(task: Reaction) = synchronized {
    sched.execute(task)
  }

  def tick(a: Actor) = sched.tick(a)

  def shutdown(): unit = sched.shutdown()

  def pendReaction: unit = sched.pendReaction
  def unPendReaction: unit = sched.unPendReaction
}

/**
 * This abstract class provides a common interface for all
 * schedulers used to execute reactors.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
trait IScheduler {
  def execute(task: Reaction): unit
  def getTask(worker: WorkerThread): Runnable
  def tick(a: Actor): unit

  def shutdown(): unit

  val QUIT_TASK = new Reaction() {
    def actor: Actor = null
    def run(): unit = {}
    override def toString() = "QUIT_TASK"
  }

  def pendReaction: unit
  def unPendReaction: unit
}

/**
 * The class <code>TickedScheduler</code> ...
 *
 * @author Philipp Haller
 */
class TickedScheduler extends Thread with IScheduler {
  private val tasks = new Queue[Reaction]
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]

  private val idle = new Queue[WorkerThread]
  private val ticks = new HashMap[WorkerThread, long]
  private val executing = new HashMap[Actor, WorkerThread]

  private var terminating = false

  private var pendingReactions = new Stack[unit]
  def pendReaction: unit = {
    //Debug.info("pend reaction")
    pendingReactions push ()
  }
  def unPendReaction: unit = {
    //Debug.info("unpend reaction")
    if (!pendingReactions.isEmpty)
      pendingReactions.pop
  }

  var TICKFREQ = 5
  var CHECKFREQ = 50

  def init() = {
    for (val i <- List.range(0, 2)) {
      val worker = new WorkerThread(this)
      workers += worker
      worker.start()
    }
  }
  init()

  override def run(): unit = {
    try {
      while (!terminating) {
        this.synchronized {
          try {
            wait(CHECKFREQ)
          } catch {
            case _: InterruptedException =>
              if (terminating) throw new QuitException
          }

          if (tasks.length > 0) {
            // check if we need more threads
            val iter = workers.elements
            var foundBusy = false
            while (iter.hasNext && !foundBusy) {
              val wt = iter.next
              ticks.get(wt) match {
                case None =>
                  foundBusy = false
                case Some(ts) =>
                  val currTime = Platform.currentTime
                  if (currTime - ts < TICKFREQ)
                    foundBusy = true
              }
            }

            if (!foundBusy) {
              val newWorker = new WorkerThread(this)
              workers += newWorker

              // dequeue item to be processed
              val item = tasks.dequeue

              executing.update(item.actor, newWorker)
              newWorker.execute(item)
              newWorker.start()
            }
          } // tasks.length > 0
          else {
            //Debug.info("task queue empty")
            if (pendingReactions.isEmpty) {
              //Debug.info("no pending reactions")
              // if all worker threads idle terminate
              if (workers.length == idle.length) {
                Debug.info("all threads idle, terminating")
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
    if (!terminating)
      if (idle.length > 0) {
        val wt = idle.dequeue
        executing.update(item.actor, wt)
        wt.execute(item)
      }
      else
        tasks += item
  }

  /**
   *  @param worker the worker thread executing tasks
   *  @return       the executed task
   */
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

  /**
   *  @param a the actor
   */
  def tick(a: Actor): unit = synchronized {
    ticksCnt = ticksCnt + 1
    executing.get(a) match {
      case None =>
        // thread outside of scheduler;
        // error("No worker thread associated with actor " + a)
      case Some(wt) =>
        ticks.update(wt, Platform.currentTime)
    }
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
      // caused deadlock (tries to acquire lock of worker)
      //worker.join()
    }
  }
}


/**
 * The <code>QuickException</code> class ...
 */
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
 * This scheduler executes the tasks of a reactor on a single
 * thread (the current thread).
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
class SingleThreadedScheduler extends IScheduler {
  def execute(task: Reaction): unit = {
    // execute task immediately on same thread
    task.run()
  }

  def getTask(worker: WorkerThread): Runnable = null
  def tick(a: Actor): Unit = {}
  def shutdown(): Unit = {}
  def pendReaction: unit = {}
  def unPendReaction: unit = {}
}

/**
 * This scheduler creates additional threads whenever there is no
 * idle thread available.
 *
 * @version 0.9.0
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

  def execute(task: Reaction): unit = synchronized {
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

  def tick(a: Actor): unit = {}

  def shutdown(): unit = synchronized {
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

  def pendReaction: unit = {}
  def unPendReaction: unit = {}
}


/**
 * <p>
 *   The class <code>WorkerThread</code> is used by schedulers to execute
 *   actor tasks on multiple threads.
 * </p>
 * <p>
 *   !!ACHTUNG: If you change this, make sure you understand the following
 *   proof of deadlock-freedom!!
 * </p>
 * <p>
 *   We proof that there is no deadlock between the scheduler and
 *   any worker thread possible. For this, note that the scheduler
 *   only acquires the lock of a worker thread by calling
 *   <code>execute</code>.  This method is only called when the worker thread
 *   is in the idle queue of the scheduler. On the other hand, a
 *   worker thread only acquires the lock of the scheduler when it
 *   calls <code>getTask</code>. At the only callsite of <code>getTask</code>,
 *   the worker thread holds its own lock.
 * </p>
 * <p>
 *   Thus, deadlock can only occur when a worker thread calls
 *   <code>getTask</code> while it is in the idle queue of the scheduler,
 *   because then the scheduler might call (at any time!) <code>execute</code>
 *   which tries to acquire the lock of the worker thread. In such
 *   a situation the worker thread would be waiting to acquire the
 *   lock of the scheduler and vice versa.
 * </p>
 * <p>
 *   Therefore, to prove deadlock-freedom, it suffices to ensure
 *   that a worker thread will never call <code>getTask</code> when
 *   it is in the idle queue of the scheduler.
 * </p>
 * <p>
 *   A worker thread enters the idle queue of the scheduler when
 *   <code>getTask</code> returns <code>null</code>. Then it will also stay
 *   in the while-loop W (<code>while (task eq null)</code>) until
 *   <code>task</code> becomes non-null. The only way this can happen is
 *   through a call of <code>execute</code> by the scheduler. Before every
 *   call of <code>execute</code> the worker thread is removed from the idle
 *   queue of the scheduler. Only then--after executing its task--
 *   the worker thread may call <code>getTask</code>. However, the scheduler
 *   is unable to call <code>execute</code> as the worker thread is not in
 *   the idle queue any more. In fact, the scheduler made sure
 *   that this is the case even _before_ calling <code>execute</code> and
 *   thus releasing the worker thread from the while-loop W. Thus,
 *   the property holds for every possible interleaving of thread
 *   execution. QED
 * </p>
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
class WorkerThread(sched: IScheduler) extends Thread {
  private var task: Runnable = null
  private[actors] var running = true

  def execute(r: Runnable) = synchronized {
    task = r
    notify()
  }

  override def run(): unit =
    try {
      while (running) {
        if (task ne null) {
          try {
            task.run()
          } catch {
            case consumed: InterruptedException =>
              if (!running) throw new QuitException
          }
        }
        this.synchronized {
          task = sched.getTask(this)

          while (task eq null) {
            try {
              wait()
            } catch {
              case consumed: InterruptedException =>
                if (!running) throw new QuitException
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
