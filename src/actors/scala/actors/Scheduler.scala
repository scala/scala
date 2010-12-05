/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import java.util.concurrent._
import scheduler.{DelegatingScheduler, ForkJoinScheduler, ResizableThreadPoolScheduler, ThreadPoolConfig}

/**
 * The <code>Scheduler</code> object is used by <code>Actor</code> to
 * execute tasks of an actor execution.
 *
 * @author Philipp Haller
 */
object Scheduler extends DelegatingScheduler {

  Debug.info("initializing "+this+"...")

  def makeNewScheduler: IScheduler = {
    val sched = if (!ThreadPoolConfig.useForkJoin) {
      // default is non-daemon
      val s = new ResizableThreadPoolScheduler(false)
      s.start()
      s
    } else {
      // default is non-daemon, fair
      val s = new ForkJoinScheduler
      s.start()
      s
    }
    Debug.info(this+": starting new "+sched+" ["+sched.getClass+"]")
    sched
  }

  /* Only <code>ForkJoinScheduler</code> implements this method.
   */
  @deprecated("snapshot will be removed")
  def snapshot() {
    if (sched.isInstanceOf[ForkJoinScheduler]) {
      sched.asInstanceOf[ForkJoinScheduler].snapshot()
    } else
      system.error("scheduler does not implement snapshot")
  }

  /* Only <code>ForkJoinScheduler</code> implements this method.
   */
  @deprecated("restart will be removed")
  def restart() {
    if (sched.isInstanceOf[ForkJoinScheduler]) {
      sched.asInstanceOf[ForkJoinScheduler].restart()
    } else
      system.error("scheduler does not implement restart")
  }

}
