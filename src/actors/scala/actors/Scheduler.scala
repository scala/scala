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

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

/**
 * The <code>Scheduler</code> object is used by
 * <code>Actor</code> to execute tasks of an execution of a
 * reactor.
 *
 * @version 0.9.2
 * @author Philipp Haller
 */
object Scheduler {
  private var sched: IScheduler =
    {
      var s: Thread with IScheduler = null

      // Check for JDK version >= 1.5
      var olderThanJDK5 = false
      try {
        java.lang.Class.forName("java.util.concurrent.ThreadPoolExecutor")
      } catch {
        case _: ClassNotFoundException =>
          olderThanJDK5 = true
      }

      s = if (olderThanJDK5)
        new TickedScheduler
      else {
        var corePoolSize = 4
        var maxPoolSize = 16
        val prop = java.lang.System.getProperty("actors.corePoolSize")
        if (null ne prop) {
          corePoolSize =
            Integer.parseInt(java.lang.System.getProperty("actors.corePoolSize"))
          maxPoolSize =
            Integer.parseInt(java.lang.System.getProperty("actors.maxPoolSize"))
        }
        new JDK5Scheduler(corePoolSize, maxPoolSize)
      }
      s.start()
      s
    }

  def impl = sched
  def impl_= (scheduler: IScheduler) = {
    sched = scheduler
  }

  def start(task: Reaction) = sched.start(task)
  def execute(task: Reaction) = sched.execute(task)
  def tick(a: Actor) = sched.tick(a)
  def terminated(a: Actor) = sched.terminated(a)
  def pendReaction: unit = sched.pendReaction
  def unPendReaction: unit = sched.unPendReaction

  def shutdown() = sched.shutdown()

  def onLockup(handler: () => unit) = sched.onLockup(handler)
  def onLockup(millis: int)(handler: () => unit) = sched.onLockup(millis)(handler)
  def printActorDump = sched.printActorDump
}


/**
 * This abstract class provides a common interface for all
 * schedulers used to execute reactors.
 *
 * @version 0.9.2
 * @author Philipp Haller
 */
trait IScheduler {
  def start(task: Reaction): unit
  def execute(task: Reaction): unit
  def getTask(worker: WorkerThread): Runnable
  def tick(a: Actor): unit
  def terminated(a: Actor): unit
  def pendReaction: unit
  def unPendReaction: unit

  def shutdown(): unit

  def onLockup(handler: () => unit): unit
  def onLockup(millis: int)(handler: () => unit): unit
  def printActorDump: unit

  val QUIT_TASK = new Reaction(null) {
    override def run(): unit = {}
    override def toString() = "QUIT_TASK"
  }
}


/**
 * This scheduler executes the tasks of a reactor on a single
 * thread (the current thread).
 *
 * @version 0.9.2
 * @author Philipp Haller
 */
class SingleThreadedScheduler extends IScheduler {
  def start(task: Reaction) {
    // execute task immediately on same thread
    task.run()
  }

  def execute(task: Reaction) {
    // execute task immediately on same thread
    task.run()
  }

  def getTask(worker: WorkerThread): Runnable = null
  def tick(a: Actor): Unit = {}
  def terminated(a: Actor): unit = {}
  def pendReaction: unit = {}
  def unPendReaction: unit = {}

  def shutdown(): Unit = {}

  def onLockup(handler: () => unit): unit = {}
  def onLockup(millis: int)(handler: () => unit): unit = {}
  def printActorDump: unit = {}
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
 * @version 0.9.2
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
