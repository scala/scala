/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import scheduler.{DelegatingScheduler, ForkJoinScheduler, ResizableThreadPoolScheduler, ThreadPoolConfig}

/**
 * Used by [[scala.actors.Actor]] instances to
 * execute tasks of an actor execution.
 *
 * @author Philipp Haller
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
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
}
