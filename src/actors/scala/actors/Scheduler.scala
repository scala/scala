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
import scheduler.{DelegatingScheduler, ForkJoinScheduler, ResizableThreadPoolScheduler}

/**
 * The <code>Scheduler</code> object is used by <code>Actor</code> to
 * execute tasks of an actor execution.
 *
 * @author Philipp Haller
 */
object Scheduler extends DelegatingScheduler {

  Debug.info("initializing "+this+"...")

  def makeNewScheduler: IScheduler = {
    val useForkJoin = try {
      val fjProp = System.getProperty("actors.enableForkJoin")
      if (fjProp != null)
        fjProp.equals("true")
      else {
        val javaVersion = System.getProperty("java.version")
        val jvmVendor =   System.getProperty("java.vm.vendor")
        Debug.info(this+": java.version = "+javaVersion)
        Debug.info(this+": java.vm.vendor = "+jvmVendor)
        (javaVersion.indexOf("1.6") != -1 ||
         javaVersion.indexOf("1.7") != -1) &&
        // on IBM J9 1.6 do not use ForkJoinPool
        (jvmVendor.indexOf("Sun") != -1)
      }
    } catch {
      case se: SecurityException => false
    }
    val sched = if (!useForkJoin) {
      val s = new ResizableThreadPoolScheduler(false)
      s.start()
      s
    } else {
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
      error("scheduler does not implement snapshot")
  }

  /* Only <code>ForkJoinScheduler</code> implements this method.
   */
  @deprecated("restart will be removed")
  def restart() {
    if (sched.isInstanceOf[ForkJoinScheduler]) {
      sched.asInstanceOf[ForkJoinScheduler].restart()
    } else
      error("scheduler does not implement restart")
  }

}
