/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.util.concurrent._
import scheduler.{DelegatingScheduler, ThreadPoolConfig, ThreadPoolScheduler, ForkJoinScheduler}

/**
 * The <code>Scheduler</code> object is used by <code>Actor</code> to
 * execute tasks of an execution of an actor.
 *
 * @author Philipp Haller
 */
object Scheduler extends DelegatingScheduler {

  Debug.info("initializing "+this+"...")

  def makeNewScheduler: IScheduler = {
    val workQueue = new LinkedBlockingQueue[Runnable]
    val threadPool = new ThreadPoolExecutor(ThreadPoolConfig.corePoolSize,
                                            ThreadPoolConfig.maxPoolSize,
                                            60000L,
                                            TimeUnit.MILLISECONDS,
                                            workQueue,
                                            new ThreadPoolExecutor.CallerRunsPolicy)
    val s = new ThreadPoolScheduler(threadPool, true)
    //val s = new ForkJoinScheduler
    Debug.info(this+": starting new "+s+" ["+s.getClass+"]")
    s.start()
    s
  }

  /* Only <code>ForkJoinScheduler</code> implements this method.
   */
  @deprecated def snapshot() {
    if (sched.isInstanceOf[ForkJoinScheduler]) {
      sched.asInstanceOf[ForkJoinScheduler].snapshot()
    } else
      error("scheduler does not implement snapshot")
  }

  /* Only <code>ForkJoinScheduler</code> implements this method.
   */
  @deprecated def restart() {
    if (sched.isInstanceOf[ForkJoinScheduler]) {
      sched.asInstanceOf[ForkJoinScheduler].restart()
    } else
      error("scheduler does not implement restart")
  }

}
