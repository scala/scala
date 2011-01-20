/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors
package scheduler

/**
 * Default scheduler for actors with daemon semantics, such as those backing futures.
 *
 * @author Erik Engbrecht
 */
object DaemonScheduler extends DelegatingScheduler {

  protected def makeNewScheduler(): IScheduler = {
    val sched = if (!ThreadPoolConfig.useForkJoin) {
      val s = new ResizableThreadPoolScheduler(true)
      s.start()
      s
    } else {
      val s = new ForkJoinScheduler(true)
      s.start()
      s
    }
    Debug.info(this+": starting new "+sched+" ["+sched.getClass+"]")
    sched
  }

}
