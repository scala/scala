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
import java.lang.Runnable

/**
 * The <code>Scheduler</code> object is used by <code>Actor</code> to
 * execute tasks of an execution of an actor.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
object Scheduler extends DelegatingScheduler {

  Debug.info("initializing "+this+"...")

  def makeNewScheduler: IScheduler = {
    val sched = new DefaultExecutorScheduler
    sched.start()
    sched
  }

  private var tasks: LinkedQueue = null

  /* Assumes <code>sched</code> holds an instance
   * of <code>FJTaskScheduler2</code>.
   */
  def snapshot(): Unit = synchronized {
    if (sched.isInstanceOf[FJTaskScheduler2]) {
      val fjts = sched.asInstanceOf[FJTaskScheduler2]
      tasks = fjts.snapshot()
      fjts.shutdown()
    } else
      error("snapshot operation not supported.")
  }

  /** Shuts down the current scheduler and creates and starts a new scheduler.
   *
   *  If the current scheduler is an <code>FJTaskScheduler2</code>
   *  a new scheduler of the same class is created. In that case,
   *  tasks resulting from a <code>snapshot</code> are
   *  submitted for execution.
   *
   *  If the current scheduler is not an <code>FJTaskScheduler2</code>,
   *  a <code>DefaultExecutorScheduler</code> is created.
   */
  def restart(): Unit = synchronized {
    // 1. shut down current scheduler
    if (sched ne null)
      sched.shutdown()

    // 2. create and start new scheduler
    if ((sched ne null) && sched.isInstanceOf[FJTaskScheduler2]) {
      sched = {
        val s = new FJTaskScheduler2
        s.start()
        s
      }
      if (tasks != null) {
        while (!tasks.isEmpty()) {
          sched.execute(tasks.take().asInstanceOf[FJTask])
        }
        tasks = null
      }
    } else {
      sched = {
        val s = new DefaultExecutorScheduler
        s.start()
        s
      }
    }
  }

}
